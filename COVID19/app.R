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
## The datasets are pulled from Johns Hopkins University
## CSSE COVID19 github repository
##
## File Name: time_series_covid19_confirmed_global.csv
## File Name:  time_series_covid19_deaths_global.csv
## File Name:  time_series_covid19_recovered_global.csv

# Confirmed cases are aggregated by country
confirmed_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
  clean_coviddat()


recovered_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
  clean_coviddat()
      

deaths_dat <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  clean_coviddat()
  




### MAP DATA ###
## The dataset is pulled from the R package {maps}
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



## Compare country names in the COVID19 datasets and the map dataset
#diff <- data.frame(unique(confirmed_dat[!confirmed_dat$country_region%in%map_dat$country_region,]$country_region))

#map_country <- data.frame(map_dat$country_region)
### 20 unmatched countries in COVID19 dataset that are NOT in the map dataset

#1. COVID 19: antigua and barbuda   
## Map: antigua; barbuda
map_dat$country_region[tolower(map_dat$country_region) == "antigua"] <- "antigua and barbuda"
map_dat$country_region[tolower(map_dat$country_region) == "barbuda"] <- "antigua and barbuda"

##2. COVID19: burma
## Map: myanmar
map_dat$country_region[tolower(map_dat$country_region) == "myanmar"] <- "burma"



#3. COVID19: cabo verde  
## Map: cape verde
map_dat$country_region[tolower(map_dat$country_region) == "cape verde"] <- "cabo verde"



#4. COVID19: congo (brazzaville)  
## Map: republic of congo
map_dat$country_region[tolower(map_dat$country_region) == "republic of congo"] <- "congo (brazzaville)"


#5. COVID19: congo (kinshasa)   
## Map: democratic republic of congo

map_dat$country_region[tolower(map_dat$country_region) == "democratic republic of the congo"] <- "congo (kinshasa)"


#6. COVID19: cote d'ivoire
## Map: ivory coast
map_dat$country_region[tolower(map_dat$country_region) == "ivory coast"] <- "cote d'ivoire"



#7. COVID19: czechia
# Map: czech republic
map_dat$country_region[tolower(map_dat$country_region) == "czech republic"] <- "czechia"


#8. COVID19: diamond princess
# Map: NONE
# Not going to be mapped as it is not a country

#9. COVID19: eswatini
# Map: swaziland
map_dat$country_region[tolower(map_dat$country_region) == "swaziland"] <- "eswatini"

#10. COVID19: holy see
# Map: No Match


#11. COVID19: korea, south
## Map: south korea
map_dat$country_region[tolower(map_dat$country_region) == "south korea"] <- "korea, south"


#12. COVID19: ms zaandam
## Map: NONE
# Not going to be mapped as it is not a country

#13. COVID19: north macedonia
## Map: macedonia
map_dat$country_region[tolower(map_dat$country_region) == "macedonia"] <- "north macedonia"

#14. COVID19: saint kitts and nevis
## Map: saint kitts; nevis

map_dat$country_region[tolower(map_dat$country_region) == "saint kitts"] <- "saint kitts and nevis"
map_dat$country_region[tolower(map_dat$country_region) == "nevis"] <- "saint kitts and nevis"


#15. COVID19: saint vincent and the grenadines
## Map: saint vincent; grenadines

map_dat$country_region[tolower(map_dat$country_region) == "saint vincent"] <- "saint vincent and the grenadines"
map_dat$country_region[tolower(map_dat$country_region) == "grenadines"] <- "saint vincent and the grenadines"


#16. COVID19: taiwan*
## Map: taiwan

map_dat$country_region[tolower(map_dat$country_region) == "taiwan"] <- "taiwan*"



#17. COVID19: trinidad and tobago
## Map: trinidad; tobago

map_dat$country_region[tolower(map_dat$country_region) == "trinidad"] <- "trinidad and tobago"

map_dat$country_region[tolower(map_dat$country_region) == "tobago"] <- "trinidad and tobago"


#18. COVID19: united kingdom
## Map: uk

map_dat$country_region[tolower(map_dat$country_region) == "uk"] <- "united kingdom"

#19. COVID19: us
## Map: usa

map_dat$country_region[tolower(map_dat$country_region) == "usa"] <- "us"

#20. COVID19: west bank and gaza (no palestine in the dataset)
## Map: palestine

map_dat$country_region[tolower(map_dat$country_region) == "palestine"] <- "west bank and gaza"





## Joing COVID19 datasets and map dataset
start_day = as.Date(confirmed_dat$date[1], "%m-%d-%y")
end_day = as.Date(confirmed_dat$date[nrow(confirmed_dat)], "%m-%d-%y")

#no_coviddat <- anti_join(map_dat, confirmed_dat, by = "country_region")



confirmed <- left_join(map_dat, confirmed_dat, by = "country_region") %>% 
  complete(date = seq(start_day, end_day, by = "days"), nesting(country_region, ID))


confirmed_geo <- left_join(confirmed, map_dat, by = "country_region") %>%
  select(ID.x, country_region, date, cases, categ, geom.y)

start_day = as.Date(recovered_dat$date[1], "%m-%d-%y")
end_day = as.Date(recovered_dat$date[nrow(confirmed_dat)], "%m-%d-%y")


recovered <- left_join(map_dat, recovered_dat, by = "country_region") %>%
  complete(date = seq(start_day, end_day, by = "days"), nesting(country_region, ID))

recovered_geo <- left_join(recovered, map_dat, by = "country_region") %>%
  select(ID.x, country_region, date, cases, categ, geom.y)

deaths <- left_join(map_dat, deaths_dat, by = "country_region") %>%
  complete(date = seq(start_day, end_day, by = "days"), nesting(country_region, ID))

deaths_geo <- left_join(deaths, map_dat, by = "country_region") %>%
  select(ID.x, country_region, date, cases, categ, geom.y)


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
                  min = as.Date(confirmed_dat$date[1], "%m-%d-%y"), 
                  max = as.Date(confirmed_dat$date[nrow(confirmed_dat)], "%m-%d-%y"), 
                  value = as.Date(confirmed_dat$date[nrow(confirmed_dat)], "%m-%d-%y"),
                  timeFormat = "%m-%d-%y",
                  animate = TRUE)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("map", width = "100%")),
                  tabPanel("Top Ten Countries", plotOutput("topten", width = "100%")),
                  tabPanel("References", htmlOutput("datasource"), htmlOutput("colorref"), htmlOutput("others"))
                  
                  )
      
              
              
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  ## Map output
  output$map <- renderPlot({
    
    
    dat <- switch(input$var, 
                   "Confirmed Cases" = confirmed_geo,
                   "Recovered Cases" = recovered_geo,
                   "Deaths Cases" = deaths_geo)
    
    color <- switch(input$var,
                    "Confirmed Cases" = c(
                      "0-199" = "#FFFFB2",
                      "200-999" = "#FED976",
                      "1,000-9,999" = "#FD8D3C",
                      "10,000-499,999" = "#F03B20",
                      "500,000+" = "#BD0026"),
                    
                    "Recovered Cases" = c(
                      "0-199" = "#D9F0A3",
                      "200-999" = "#ADDD8E",
                      "1,000-9,999" = "#78C679",
                      "10,000-499,999" = "#238443",
                      "500,000+" = "#005A32"),
                    
                    "Deaths Cases" = c(
                      "0-199" = "#D0D1E6",
                      "200-999" = "#A6BDDB",
                      "1,000-9,999" = "#67A9CF",
                      "10,000-499,999" = "#1C9099",
                      "500,000+" = "#016C59"))

   
    covid_map(dat, input$date, input$var, color)
    
    
  },
  height = 600, width = 800
  )
  
  
  ## Summary plot output
  output$topten <- renderPlot({
    
    dat <- switch(input$var, 
                  "Confirmed Cases" = confirmed_geo,
                  "Recovered Cases" = recovered_geo,
                  "Deaths Cases" = deaths_geo)
    
    color <- switch(input$var,
                    "Confirmed Cases" = c(
                      "0-199" = "#FFFFB2",
                      "200-999" = "#FED976",
                      "1,000-9,999" = "#FD8D3C",
                      "10,000-499,999" = "#F03B20",
                      "500,000+" = "#BD0026"),
                    
                    "Recovered Cases" = c(
                      "0-199" = "#D9F0A3",
                      "200-999" = "#ADDD8E",
                      "1,000-9,999" = "#78C679",
                      "10,000-499,999" = "#238443",
                      "500,000+" = "#005A32"),
                    
                    "Deaths Cases" = c(
                      "0-199" = "#D0D1E6",
                      "200-999" = "#A6BDDB",
                      "1,000-9,999" = "#67A9CF",
                      "10,000-499,999" = "#1C9099",
                      "500,000+" = "#016C59"))
    
    
    
    
    top_ten(dat, input$var, input$date, color)},
    
    height = 800, width = 600
    
    )
  
  
  ## Memo output
  
  output$datasource <- renderUI({
    HTML("<strong>Data Sources: </strong><br/>
    
             <p>1. COVID19 datasets used in this project come from Johns Hopkins CSSE's github repository: <br/>
             
              COVID 19 Data for Confirmed Cases: time_series_covid19_confirmed_global.csv <br/>
    
              COVID 19 Data for Recovered Cases: time_series_covid19_recovered_global.csv <br/>
    
              COVID 19 Data for Deaths Cases:  time_series_covid19_deaths_global.csv </p>
              
              <p>2.Map datasets used in this project come from the R package, {maps}.</p>
         
              <p>Note: COVID 19 data for confirmed cases, recovered cases, and deaths cases are cumulative.")
    
    
  })
  
  output$colorref <-renderUI({
    HTML("<strong>Color References: </strong><br/>
    
    <p>1. Colors used in this project were inspired by colors used in the following graphics found in <a href = 'https://ourworldindata.org/smoking'>Our World in Data</a>: <br/>
    
    Colors for Confirmed Cases: Death Rate From Smoking <br/>
    
    Colors for Recovered Cases: Taxes as a Share of Cigarette Price <br/>
    
    Colors for Deaths Cases: Average Price of a Pack of Cigarettes</p>")
  })
  
  
  output$others <- renderUI({
    HTML("<strong>Cutoff Values References: </strong><br/>
    <p>1. Cutoff values used in this project were referenced to the values used in the example found in <a href = 'https://datascienceplus.com/map-visualization-of-covid19-across-world'>Data Science Plus: visualize COVID19 across world</a>.</p>
    
    <strong>Theme References: </strong><br/>
    <p>1. Theme used in this project was referenced to the theme used in the example found in  <a href = 'https://datascienceplus.com/map-visualization-of-covid19-across-world'>Data Science Plus: visualize COVID19 across world</a>.</p>")
  })
}

# Run app ----
shinyApp(ui, server)