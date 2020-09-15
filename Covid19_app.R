#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####### Setup
library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)
select <- dplyr::select

#Time_series_orig <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", delim = ",")
Time_series_orig <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", delim = ",")
#Time_series_deaths_orig <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", delim = ",")
Time_series_deaths_orig <- read_delim("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", delim = ",")
School_closures <- read_delim("https://en.unesco.org/sites/default/files/covid_impact_education.csv", delim= ",")

tbl_incl_test_nrs <- read_delim("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", delim = ",")

Covid19_confirmed <- 
  Time_series_orig %>%
  gather("Date", "Confirmed_cases", `1/22/20`:colnames(Time_series_orig)[length(Time_series_orig)]) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%y"))

Covid19_deaths <- 
  Time_series_deaths_orig %>% 
  gather("Date", "Deaths", `1/22/20`:colnames(Time_series_orig)[length(Time_series_orig)]) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%y"))

Covid19 <- left_join(Covid19_confirmed, Covid19_deaths) 

Covid19_by_country <- Covid19 %>%
  group_by("Country_Region"= `Country/Region`, Date) %>%
  dplyr::summarise(cases_per_country = sum(Confirmed_cases), deaths_per_country = sum(Deaths)) %>%
  ungroup() %>%
  filter(cases_per_country != 0) %>%
  mutate(percentage_deaths_per_confirmed_cases= deaths_per_country/cases_per_country*100 )


StartDate <- Covid19_by_country %>%
  filter(cases_per_country > 100) %>%
  group_by(Country_Region) %>%
  arrange(cases_per_country) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(Country_Region, StartDate=Date)

StartDate_deaths <- Covid19_by_country %>%
  filter(deaths_per_country > 2) %>%
  group_by(Country_Region) %>%
  arrange(cases_per_country) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(Country_Region, StartDate_deaths=Date)

Covid19_by_country <- 
  left_join(Covid19_by_country , StartDate) %>%
  left_join(. , StartDate_deaths) %>%
  mutate(Days_since_100th_case = difftime( Date, StartDate, units = "days"),
         Days_since_third_death = difftime( Date, StartDate_deaths, units = "days"))

Covid19_by_country <- Covid19_by_country %>%
  group_by(Country_Region) %>%
  mutate(cases_per_day_country = cases_per_country - lag(cases_per_country, order_by = Date),
         deaths_per_day_country = deaths_per_country - lag(deaths_per_country, order_by = Date)) %>%
  ungroup()

School_closures <- School_closures %>% 
  filter(Status == "Closed due to COVID-19") %>%
  mutate(Date = as.Date(Date, format="%d/%m/%y")) %>%
  group_by(Country) %>%
  arrange(Date) %>%
  slice(1) %>%
  ungroup()

School_closures <- School_closures %>%
  mutate( Country = if_else(Country == "Argentina ", "Argentina", Country )) %>%
  mutate( Country = if_else(Country == "United Kingdom of Great Britain and Northern Ireland", 
                            "United Kingdom", Country )) %>%
  mutate( Country = if_else(Country == "Iran (Islamic Republic of)", "Iran", Country )) %>%
  mutate( Country = if_else(Country == "Namibia  ", "Namibia", Country )) %>%
  mutate( Country = if_else(Country == "Pakistan ", "Pakistan", Country )) %>%
  mutate( Country = if_else(Country == "Paraguay ", "Paraguay", Country )) %>%
  mutate( Country = if_else(Country == "Republic of Korea", "Korea, South", Country )) %>%
  mutate( Country = if_else(Country == "Bolivia ", "Bolivia", Country )) %>%
  mutate( Country = if_else(Country == "Burkina Faso ", "Burkina Faso", Country )) %>%
  mutate( Country = if_else(Country == "China, Hong Kong Special Administrative Region", 
                            "Hong Kong", Country ))
School_closures <- School_closures %>% dplyr::select(Date, Country) %>% mutate(Measures = "School closure" )

Covid19_by_country <- left_join(Covid19_by_country, School_closures, 
                                by=c("Country_Region"= "Country", "Date"="Date"))

Date_school_closures <- Covid19_by_country %>% filter(Measures == "School closure") %>%
  dplyr::select(Country_Region, "date_school_closure"=Date)

Covid19_by_country <- left_join(Covid19_by_country , Date_school_closures) %>%
  mutate(Days_since_schoolclosures = difftime( Date, date_school_closure, units = "days"))

###### UI
ui <- fluidPage(
  
  # Application title
  titlePanel(paste0("Covid-19 (last updated ", max(Covid19_by_country$Date), ")" )),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter", 
                  label = "parameter", 
                  choices = c("cases_per_country", "percentage_deaths_per_confirmed_cases", "deaths_per_country",
                              "cases_per_day_country", "deaths_per_day_country"), 
                  selected = "cases_per_country" ),
      selectInput("time_scale", label = "Relativ time scale since outbreak", 
                  choices = c("Days_since_100th_case", "Date", "Days_since_third_death",
                              "Days_since_schoolclosures"), 
                  selected = "Date" ),
      checkboxInput("scale", label = "log10", value = FALSE),
      sliderInput("minNrcases",
                  "Number of cases:",
                  min = 1,
                  max = max(Covid19_by_country$cases_per_country),
                  value = c(25000,
                            max(Covid19_by_country$cases_per_country))),
      selectInput("continent", 
                  label = "continent", 
                  choices = c("all", "Africa", "Americas", "Asia", "Europe",
                              "Oceania"), 
                  selected = "all" ),
      checkboxInput("sel_country", label = "Only selected countries", value = FALSE),
      selectInput('countries', 'Countries', unique(Covid19_by_country$Country_Region), 
                  multiple=TRUE, selectize=TRUE, selected = "Germany")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot_since_outbreak")
    )
  )
)

# Server
server <- function(input, output){
  
  settings <- reactive({
    countries_with_min_cases <- Covid19 %>%
      filter(Date == max(Covid19$Date)) %>%
      group_by(`Country/Region`) %>%
      dplyr::summarise(cases_per_country = sum(Confirmed_cases)) %>%
      filter(cases_per_country <= input$minNrcases[2] & 
               cases_per_country >= input$minNrcases[1] ) %>% .$`Country/Region`
    
    if(input$continent!="all"){
      # Add continent to data frame
      continent <- countrycode(sourcevar = countries_with_min_cases,
                                                  origin = "country.name",
                                                  destination = "continent")
      countries_with_min_cases <- countries_with_min_cases[continent == input$continent]
    }
    
    if(input$sel_country==TRUE){
      countries_with_min_cases <- input$countries
    }
    
    if(input$time_scale =="Days_since_100th_case"){ 
      df_cov <- Covid19_by_country %>% 
        filter(Country_Region %in% countries_with_min_cases,
               Days_since_100th_case >=0)
      xparam =  "Days_since_100th_case"
    }else if(input$time_scale== "Date"){
      df_cov <- Covid19_by_country %>% 
        filter(Country_Region %in% countries_with_min_cases)
      xparam =  "Date" 
    }else if(input$time_scale == "Days_since_third_death"){
      df_cov <- Covid19_by_country %>% 
        filter(Country_Region %in% countries_with_min_cases ,
               Days_since_third_death >=0)
      xparam = "Days_since_third_death"
    }else if(input$time_scale == "Days_since_schoolclosures"){
      df_cov <- Covid19_by_country %>% 
        filter(Country_Region %in% countries_with_min_cases ,
               Days_since_schoolclosures >=0)
      xparam = "Days_since_schoolclosures"
    }
    
    ymax_cov <- max( df_cov[as.vector(df_cov[,input$parameter] != "Inf"),input$parameter]  
                     , na.rm = TRUE )
    
    if(input$scale & input$parameter %in% c("cases_per_country" ) & 
       input$time_scale == "Days_since_100th_case" ){
      scale_y_cov = list(scale_y_log10()) 
      ymin_cov = 100
    }else if(input$scale & input$parameter %in% c("deaths_per_country", "deaths_per_day_country",
                                                  "cases_per_day_country", "cases_per_country" )){
      scale_y_cov = list(scale_y_log10()) 
      ymin_cov = 1
    }else{
      scale_y_cov = list(scale_y_continuous())
      ymin_cov = 0
    }
    
    combo <- list(countries_with_min_cases = countries_with_min_cases, 
                  df_cov = df_cov,
                  xparam = xparam,
                  ymax_cov = ymax_cov,
                  scale_y_cov = scale_y_cov,
                  ymin_cov = ymin_cov
    )
    combo
  })
  
  ########
  
  cov_lineplot <- reactive({
    combo <- settings()
    countries_with_min_cases <- combo$countries_with_min_cases
    df_cov = combo$df_cov
    xparam = combo$xparam
    ymax_cov = combo$ymax_cov
    scale_y_cov = combo$scale_y_cov
    ymin_cov = combo$ymin_cov
    
    p <- df_cov %>%
      ggplot(aes_string(xparam , input$parameter , group= "Country_Region", 
                        color = "Country_Region" )) +
      geom_text(
        data = df_cov %>% group_by(Country_Region) %>% 
          arrange(desc(Date)) %>%
          slice(1) %>%
          ungroup(), nudge_x = 1,
        aes_string(x = xparam, y = input$parameter, label="Country_Region", 
                   color= "Country_Region" ), size=2.5
      ) +
      geom_line( alpha=0.7) +
      scale_y_cov +
      theme_bw() +
      coord_cartesian(
        ylim=c(ymin_cov, ymax_cov))+ 
      geom_point(data= df_cov %>% filter( Measures != "NA" ), 
                 aes_string(xparam , input$parameter , #shape="Measures",
                            group= "Country_Region", color = "Country_Region" )
      ) 
    if(xparam != "Date"){ p <- p + scale_x_continuous(breaks = scales::pretty_breaks(10))
    }else{
      p <- p + scale_x_date(breaks = scales::pretty_breaks(20)) + 
        theme(axis.text.x = element_text(angle = 90))
    }
    p
    
  })
  
  cov_barplot <- reactive({
    combo <- settings()
    countries_with_min_cases <- combo$countries_with_min_cases
    df_cov = combo$df_cov
    xparam = combo$xparam
    ymax_cov = combo$ymax_cov
    scale_y_cov = combo$scale_y_cov
    ymin_cov = combo$ymin_cov
    
    p <- df_cov %>%
      ggplot(aes_string(xparam , input$parameter , group= "Country_Region", 
                        color = "Country_Region" )) +
      geom_text(
        data = df_cov %>% group_by(Country_Region) %>% 
          arrange(desc(Date)) %>%
          slice(1) %>%
          ungroup(), nudge_x = 1,
        aes_string(x = xparam, y = input$parameter, label="Country_Region", 
                   color= "Country_Region" ), size=2.5
      ) +
      geom_smooth(se = FALSE, span=0.3, alpha=0.7, size=0.5  ) +
      scale_y_cov +
      theme_bw() 
    
    if(xparam != "Date"){ p <- p + scale_x_continuous(breaks = scales::pretty_breaks(10))
    }else{
      p <- p + scale_x_date(breaks = scales::pretty_breaks(20)) + 
        theme(axis.text.x = element_text(angle = 90))
    }
    
    p
    
  })
  
  type_of_plot <- reactive({
    if(input$parameter %in% c("deaths_per_day_country", "cases_per_day_country")){
      p <- cov_barplot()
    }else{
      p <- cov_lineplot()
    }
    p
  })
  
  output$plot_since_outbreak <- renderPlotly({
    p <- type_of_plot()
    
    a <- list(showticklabels = TRUE)
    
    print(ggplotly(p))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

