## STAT302 ##
## Final Project ##
## Jingyang Zhang ##
## May 20th, 2020 ##
## app.R ##


# Load packages ----

library(shiny)

library(maps)

library(tidyverse)

library(janitor)

library(sf)


# Source helper functions -----
source("helpers.R")


# Load data ----

### COVID 19 DATA ###
## The data sets are pulled from Johns Hopkins University
## CSSE COVID19 github repository
##
## File Name: time_series_covid19_confirmed_global.csv
## File Name:  time_series_covid19_deaths_global.csv
## File Name:  time_series_covid19_recovered_global.csv

# Confirmed cases are aggregated by country
confirmed_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  clean_coviddat()
      

deaths_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  clean_coviddat()
  

recovered_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
  clean_coviddat()


### MAP DATA ###
## The data sets are pulled package {maps}
##
  
map_dat <- maps::map(
  database = "world",
  plot = FALSE,
  fill = TRUE
  ) %>%
  st_as_sf() %>%
  mutate(
    country_region = tolower(ID)
  )


confirmed_geo <- left_join(confirmed_dat, map_dat, by = "country_region")

recovered_geo <- left_join(recovered_dat, map_dat, by = "country_region") 
  
deaths_geo <- left_join(deaths_dat, map_dat, by = "country_region") 




# User interface ----
ui <- fluidPage(
  titlePanel("Covid 19 Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
  
      # Dropdown widget so user can select a state or us map
      selectInput("var", 
                  label = "Select display cases:",
                  choices = c("Confirmed Cases", 
                              "Recovered Cases", 
                              "Deaths Cases"),
                  selected = "Confirmed Cases"),
      
      sliderInput("date", 
                  label = "Date of interest:",
                  min = as.Date(confirmed_geo$date[1], "%m-%d-%y"), 
                  max = as.Date(confirmed_geo$date[nrow(confirmed_geo)], "%m-%d-%y"), 
                  value = as.Date(confirmed_geo$date[nrow(confirmed_geo)], "%m-%d-%y"),
                  timeFormat = "%m-%d-%y",
                  animate = TRUE)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("map")),
                  tabPanel("Memo", textOutput("datasource"))
                  )
      
              
              
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  ## Map output
  output$map <- renderPlot({
    
    
    mdy_list <- str_split(gsub(" 0", " ", format(as.Date(input$date), "%Y, %m, %d")), ", ")
    y <- substr(mdy_list[[1]][1], start = 3, stop = 4)
    m <- mdy_list[[1]][2]
    d <- mdy_list[[1]][3]
    
    mdy <- paste(paste(m, d, sep = "-"), y, sep = "-")
    
    
    dat <- switch(input$var, 
                   "Confirmed Cases" = confirmed_geo,
                   "Recovered Cases" = recovered_geo,
                   "Deaths Cases" = deaths_geo)
    
    color <- switch(input$var,
                    "Confirmed Cases" = c(
                      "0-19" = "#FFFFB2",
                      "20-99" = "#FED976",
                      "100-999" = "#FD8D3C",
                      "1000-49,999" = "#F03B20",
                      "50,000+" = "#BD0026"),
                    
                    "Recovered Cases" = c(
                      "0-19" = "#F0F9E8",
                      "20-99" = "#BAE4BC",
                      "100-999" = "#7BCCC4",
                      "1000-49,999" = "#43A2CA",
                      "50,000+" = "#0868AC"),
                    
                    "Deaths Cases" = c(
                      "0-19" = "#F8F8F8",
                      "20-99" = "#C0C0C0",
                      "100-999" = "#989898",
                      "1000-49,999" = "#585858",
                      "50,000+" = "#000000"))

   
    covid_map(dat, mdy, input$var, color)
    
    
  })
  
  
  ## Memo output
  
  output$datasource <- renderText({
    "Datasets used in this project come from Johns Hopkins CSSE's github repository."
  })
}

# Run app ----
shinyApp(ui, server)