
# Load packages ----

library(shiny)

library(tidyverse)

library(maps)

library(janitor)

library(readr)

library(viridis)


# Load data ----

### COVID 19 DATA ###
## The data sets are pulled from Johns Hopkins University
## CSSE COVID19 github repository
##
## File Name: time_series_covid19_confirmed_global.csv
## File Name:  time_series_covid19_deaths_global.csv
## File Name:  time_series_covid19_recovered_global.csv

confirmed_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  clean_names() %>%
  mutate(
    country_region = tolower(country_region)
  )


deaths_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  clean_names() %>%
  mutate(
    country_region = tolower(country_region)
  )


recovered_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))%>%
  clean_names() %>%
  mutate(
    country_region = tolower(country_region)
  )

dat_dates <- as.Date(str_replace(names(confirmed_dat[, -c(1:4)]), "x", ""), "%m_%d_%y")


## Aggregate by country 
confirmed_agg <- confirmed_dat %>%
  group_by(country_region) %>%
  summarise_at(vars(starts_with('x')), sum)

recovered_agg <- recovered_dat %>%
  group_by(country_region) %>%
  summarise_at(vars(starts_with('x')), sum)


deaths_agg <- deaths_dat %>%
  group_by(country_region) %>%
  summarise_at(vars(starts_with('x')), sum)

## Country Centroid Data
## The data set is downloaded 
## from https://worldmap.harvard.edu/data/geonode:country_centroids_az8
## on May 29th, 2020

country_cent <- read_csv("data/country_centroids_az8.csv") %>%
  clean_names() %>%
  select(name_long, longitude, latitude) %>%
  mutate(country_region = tolower(name_long))



## Joing country centroid and COVID 19 data

confirmed_geodat <- left_join(confirmed_agg, 
                              country_cent, 
                              by = "country_region")

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  


  titlePanel("COVID 19 Visualization Project"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Confirmed Cases", 
                              "Recovered Cases",
                              "Deaths Cases"),
                  selected = "Confirmed Cases"),
      
      sliderInput("date", 
                  label = "Date of interest:",
                  min = as.Date(dat_dates[1]), 
                  max = as.Date(dat_dates[length(dat_dates)]), 
                  value = as.Date(dat_dates[1]),
                  timeFormat = "%m-%d-%y",
                  animate = TRUE)
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    
    dat <- switch(input$var, 
                   "Confirmed Cases" = confirmed_geodat,
                   "Recovered Cases" = recovered_geodat,
                   "Deaths Cases" = deaths_geodat)
    
    legend_title <- switch(input$var,
                    "Confirmed Cases" = "Confirmed cases\n per 100,000 people",
                    "Recovered Cases" = "Recovered cases\n per 100,000 people",
                    "Deaths Cases" = "Deaths cases\n per 100,000 people")

   
    
    covid_map(dat, legend_title, input$date)
    
  })
}

# Run app ----
shinyApp(ui, server)