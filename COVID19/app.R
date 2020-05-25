
# Load packages ----

library(shiny)

library(tidyverse)

library(RColorBrewer)

library(sf)

library(COVID19)

# Load data ----

## Global COVID19 Data
covid19_dat <- covid19() %>%
  
  select(id,
         date, 
         confirmed, 
         recovered, 
         deaths, 
         administrative_area_level_1) %>%
  
  mutate(
    confirmed_rate = confirmed / 100000,
    recovered_rate = recovered / 100000,
    deaths_rate = deaths / 100000,
    ID = administrative_area_level_1
  ) 


## Global Map Data
map_dat <- maps::map(
  database = "world",
  plot = FALSE,
  fill = TRUE
) %>%
  st_as_sf()


# Merge COVID19 data and map data
covid19_map_dat <- left_join(covid19_dat, map_dat, by = "ID")



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
                  label = "Range of interest:",
                  min = as.Date(min(covid19_map_dat$date), 
                                "%Y-%m-%d"), 
                  max = as.Date(max(covid19_map_dat$date), 
                                "%Y-%m-%d"), 
                  value = as.Date(max(covid19_map_dat$date)),
                  timeFormat = "%Y-%m-%d")
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    
    args <- switch(input$var, 
                   "Confirmed Cases" = list("confirmed", "YlOrRd", "Confirmed Cases Per\n100,000 People"),
                   "Recovered Cases" = list("recovered", "YlGnBu", "Recovered Cases Per\n100,000 People"),
                   "Deaths Cases" = list("deaths", "Greys", "Deaths Cases Per\n100,000 People"))
    
   
  
    covid_map(covid19_map_dat, unlist(args[1]), unlist(args[2]), unlist(args[3]), input$date)
    
  })
}

# Run app ----
shinyApp(ui, server)