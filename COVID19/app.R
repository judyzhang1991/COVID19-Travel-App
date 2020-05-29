
# Load packages ----

library(shiny)

library(tidyverse)

library(RColorBrewer)

library(sf)

library(janitor)

library(readr)

library(viridis)

library(lubridate)

library(glue)

# Load data ----

### COVID 19 DATA ###
## The data sets are pulled from Johns Hopkins University
## CSSE COVID19 github repository
##
## File Name: time_series_covid19_confirmed_global.csv
## File Name:  time_series_covid19_deaths_global.csv
## File Name:  time_series_covid19_recovered_global.csv

confirmed_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  clean_names()


deaths_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  clean_names()


recovered_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))%>%
  clean_names()

dat_dates <- as.Date(str_replace(names(confirmed_dat[, -c(1:4)]), "x", ""), "%m_%d_%y")


  
### MAP DATA ###

map_dat <- maps::map(
  database = "world",
  plot = FALSE,
  fill = TRUE
)


# Cutoffs based on the number of cases
val_breaks <- c(1, 20 , 100, 1000, 50000)

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  
  # Override CSS
  tags$style(type = 'text/css',
             ".irs-grid-text {font-size: 10pt;}"),
  


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
                   "Confirmed Cases" = confirmed_dat,
                   "Recovered Cases" = recovered_dat,
                   "Deaths Cases" = deaths_dat)
    
    legend_title <- switch(input$var,
                    "Confirmed Cases" = "Confirmed cases\n per 100,000 people",
                    "Recovered Cases" = "Recovered cases\n per 100,000 people",
                    "Deaths Cases" = "Deaths cases\n per 100,000 people")

   
    
    covid_map(dat, legend_title)
    
  })
}

# Run app ----
shinyApp(ui, server)