
## COVID19 Network Spatial Visualization
## Jingyang Zhang
## network.R






# Load data

## TRAVEL RESTRICTION DATA ##
travel_rest_url <- url("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c8_internationaltravel.csv")

travel_rest_dat <- read_csv(travel_rest_url) %>%
  select(-X2)
 

colnames(travel_rest_dat)[1] <- "country"


travel_rest_dat <- travel_rest_dat %>%
  pivot_longer(cols = -country,
               names_to = "date",
               values_to = "restriction")



## FLIGHT ROUTES DATA ##
## Source: https://www.kaggle.com/open-flights/flight-route-database
routes <- read_csv("data/routes.csv") %>%
  clean_names() 

names(routes)[names(routes) == "destination_apirport"] <- "destination_airport"


## Get direct routes
direct_routes <- routes %>% filter(
  stops == 0
)

## Source: https://www.kaggle.com/divyanshrai/openflights-airports-database-2017
airports <- read_csv("data/airports.csv") %>%
  clean_names() %>%
  filter(
    iata != "\\N"
  ) %>% as.data.frame()

rownames(airports) <- airports$iata

# Create a lookup hash table for looking up country of an airport
airports_hash <- hash()

for(i in 1:nrow(airports)){
  airports_hash[[airports$iata[i]]] <- airports$country[i]
}


# Helper Function: convert date from the form 01jan2020 to 01-01-2020 ----
## args:
## date: string of date in the form of 01jan2020
## return:
## formatted_date: string of date in the form of 01-01-2020

format_date_str <- function(date){
  
  str <- toString(dmy(date))
  
  splits <- strsplit(str, "-")
  
  yr <- unlist(splits[[1]][1])
  
  month <- unlist(splits[[1]][2])
  
  day <- unlist(splits[[1]][3])
  
  formated_date <- paste(paste(month, day, sep = "-"), yr, sep = "-")
  
  return(formated_date)
}


# Reformat date column in travel restriction data

for(i in 1:nrow(travel_rest_dat)){
  travel_rest_dat$date[i] <- format_date_str(travel_rest_dat$date[i])
}



# Helper Function: check international travel restriction status ----
## args:
## travel_rest_dat: travel restriction dataset
## country_name: the country of interest
## date_str: the date of interest, format 01-22-2020
## return:

## travel_rest_status: an indicator (double) of travel restriction status:
## 0: no measures
## 1: screening
## 2: quarantine arrivals from high-risk regions
## 3: ban on high-risk regions
## 4: total border closure
## No data: blank

check_travel_rest <- function(travel_rest_dat, country_name, date_str){
  rest_status <- travel_rest_dat %>%
    filter(country == country_name & date == date_str)
  
  return(as.double(rest_status$restriction))
}


# Helper Function: update flight routes information according to the travel restriction status----
## args:
## flight_dat: flight routes data,
## travel_rest_dat: travel restriction dataset
## country_name: country of interest
## date_str: date of interest
## return
## updated_flight_dat: updated flight routes data

update_flight <- function(flight_dat, travel_rest_dat, country_name, date_str){
  
  travel_rest_status <- check_travel_rest(travel_rest_dat, country_name, date_str)
  if(travel_rest_status == 4){
    
    # If travel restriction = 4: total border closure, then remove all flights into given country
    flight_dat <- flight_dat[!(flight_dat$dest_country == country_name),]
  }
  
  return(flight_dat)
  
}


# Helper Function: find country for a given IATA code ----
## args:
## airports_hash: hastable of key = IATA and value = country name
## IATA: IATA code string
## return:
## country: country name of the given IATA code


country_lookup <- function(airports_hash, iata){
  
  return(airports_hash[[iata]])
}




# Create a lookup hash table for looking up country latitude
airport_lat_hash <- hash()

for(i in 1:nrow(airports)){
  airport_lat_hash[[airports$iata[i]]] <- airports$latitude[i]
}



# Create a lookup hash table for looking up country longtitude
airport_long_hash <- hash()

for(i in 1:nrow(airports)){
  airport_long_hash[[airports$iata[i]]] <- airports$longitude[i]
}



# Helper Function: find country for a given IATA code ----
## args:
## airports_hash: hastable of key = IATA and value = country name
## IATA: IATA code string
## return:
## country: country name of the given IATA code


country_lookup <- function(airports_hash, iata){
  
  return(airports_hash[[iata]])
}


# Helper Function: find latitude or longitude of a given airport ----
## args:
## airport_lat_hash: hastable of key = airport iata, value = latitude
## airport_long_hash: hashtable of key = airport iata, value = longitude
## iata: iata of the airport
## return:
## lat or long: latitude or longitude of the given airport


geo_lookup <- function(airport_lat_hash = NULL, airport_long_hash = NULL, iata){
  if(!is.null(airport_lat_hash)){
    
    return(airport_lat_hash[[iata]])
  }
  
  if(!is.null(airport_long_hash)){
    
    return(airport_long_hash[[iata]])
  }
  
  
}


# Add source country and destination country to direct_routes dataframe
## Initialize an empty column

# Add source country geo code and destination country geocode to direct_routes dataframe
## Initialize an empty column

direct_routes <- direct_routes %>% 
  
  mutate(
    source_country = NA,
    
    dest_country = NA,
    
    source_airport_lat = NA,
    
    source_airport_long = NA,
    
    dest_airport_lat = NA,
    
    dest_airport_long = NA
  )




for(i in 1:nrow(direct_routes)){
  
  source = country_lookup(airports_hash, direct_routes$source_airport[i])
  
  
  if(!is.null(source)){
    
    direct_routes$source_country[i] = source
    
    source_lat = geo_lookup(airport_lat_hash, NULL, direct_routes$source_airport[i])
    
    source_long = geo_lookup(NULL, airport_long_hash, direct_routes$source_airport[i])
    
    if(!is.null(source_lat) & !is.null(source_long)){
      
      direct_routes$source_airport_lat[i] = source_lat
      
      direct_routes$source_airport_long[i] = source_long
    }
    
  }
  
  dest = country_lookup(airports_hash, direct_routes$destination_airport[i])
  
  if(!is.null(dest)){
    
    direct_routes$dest_country[i] = dest
    
    dest_lat = geo_lookup(airport_lat_hash, NULL, direct_routes$destination_airport[i])
    
    dest_long = geo_lookup(NULL, airport_long_hash, direct_routes$destination_airport[i])
    
    if(!is.null(dest_lat) & !is.null(dest_long)){
      
      direct_routes$dest_airport_lat[i] = dest_lat
      
      direct_routes$dest_airport_long[i] = dest_long
    }
  }
  
}

# Remove demostic flights
direct_routes <- direct_routes %>%
  filter(source_country != dest_country)



# Helper Function: assign geo code to a country using average of latitude and longitude of all airports in the country ----
## args:
## country_name: name of the country
## direct_routes: dataframe of all international direct routes
## return:
## country_geo: calculated geo code of the country


assign_country_geo <- function(direct_routes, country_name){
  all_airports <- direct_routes %>%
    filter(source_country == country_name | dest_country == country_name)
  
  source_airports_dat <- all_airports %>% 
    filter(source_country == country_name)
  
  source_airports_lat_total <- sum(source_airports_dat$source_airport_lat)
  source_airports_long_total <- sum(source_airports_dat$source_airport_long)
  source_airports_n <- nrow(source_airports_dat)
  
  dest_airports_dat <- all_airports %>% 
    filter(dest_country == country_name)
  
  dest_airports_lat_total <- sum(dest_airports_dat$dest_airport_lat)
  dest_airports_long_total <- sum(dest_airports_dat$dest_airport_long)
  dest_airports_n <- nrow(dest_airports_dat)
  
  country_lat <- sum(source_airports_lat_total, dest_airports_lat_total) / (source_airports_n + dest_airports_n)
  country_long <- sum(source_airports_long_total, dest_airports_long_total) / (source_airports_n + dest_airports_n)
  
  return(c(country_lat, country_long))
}

# Assign geo code for each source country and each destination country
direct_routes <- direct_routes %>%
  mutate(
    source_country_lat = NA,
    source_country_long = NA,
    dest_country_lat = NA,
    dest_country_long = NA
  )



for(i in 1 : nrow(direct_routes)){
  
  source_country_geo = assign_country_geo(direct_routes, direct_routes$source_country[i])
  
  direct_routes$source_country_lat[i] = source_country_geo[1]
  direct_routes$source_country_long[i] = source_country_geo[2]
  
  dest_country_geo = assign_country_geo(direct_routes, direct_routes$dest_country[i])
  
  direct_routes$dest_country_lat[i] = dest_country_geo[1]
  direct_routes$dest_country_long[i] = dest_country_geo[2]
}


# Create a dataframe of distinct countries and its calculated geo code
countries_geo <- data.frame(
  country = unique(c(direct_routes$source_country, direct_routes$dest_country))
) %>%
  mutate(
    country_lat = NA,
    country_long = NA
  )

# Add calculated country geo code

for(i in 1:nrow(countries_geo)){
  
  countries_geo$country_lat[i] = assign_country_geo(direct_routes, countries_geo$country[i])[1]
  countries_geo$country_long[i] = assign_country_geo(direct_routes, countries_geo$country[i])[2]
  
}

rownames(countries_geo) <- countries_geo$country
  
# Plot Function: plot flight routes to and from a given country ----
## args:
## direct_routes: dataframe of all international direct routes
## country_name: name of the country
## world_map: world map ggplot object
## return:
## NULL

plot_routes <- function(direct_routes, country_name, world_map, InOut){
  
  if(InOut == "Out"){
    direct_routes <- direct_routes %>% filter(source_country == country_name)
    
    curve_color = "#16a085"
    
    dot_color = "#7303fc"
  }
  else{
    direct_routes <- direct_routes %>% filter(dest_country == country_name)
    
    curve_color = "#490e60"
    
    dot_color = "#7303fc"
  }
  
  
  
  flights <- data.frame(
    origin = direct_routes$source_country,
    destination = direct_routes$dest_country
  ) 
  
  # Convert to network
  flights <- network(flights, direct = TRUE)
  
  
  # Add geographic coordinates
  flights %v% "lat" <- countries_geo[network.vertex.names(flights), "country_lat" ]
  
  flights %v% "long" <- countries_geo[network.vertex.names(flights), "country_long" ]
  
  
  # Get country geo code
  plotcord = data.frame(
    long = as.numeric(flights %v% "long"),
    lat = as.numeric(flights %v% "lat")
  )
  
  
  # Create edges
  edges <- network::as.matrix.network.edgelist(flights)
  
  # Create edges coordinates
  edges_mat <- data.frame(
    x = plotcord$long[edges[,1]],
    xend = plotcord$long[edges[,2]],
    y = plotcord$lat[edges[,1]],
    yend = plotcord$lat[edges[,2]]
  )
  
  #plotcord$labels <- NA
  #for(i in 1:length(plotcord$long)){
  #  label = paste(plotcord$lat[i], plotcord$long[i], sep = ",")
  #  plotcord$labels[i] = label
  #}
  
  world_map + 
    geom_point(data = plotcord, mapping = aes(x = long, y = lat), 
               size = 0.008,
               color = dot_color) +
    #geom_text(data = plotcord, mapping = aes(label = labels), hjust = 0, vjust = 0)
  
    geom_curve(
      data = edges_mat, 
      mapping = aes(x = x, xend = xend, y = y, yend = yend), 
      arrow = arrow(length = unit(0.02, "npc")),
      color = curve_color
    ) + 
    theme_minimal() + 
    theme(
      
      
      ### Panel ###
      panel.background = element_rect(fill = "#ffffff", 
                                      color = NA),
      
      panel.grid = element_blank(),
      
      ### Axis ###
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      
      
      ### Legend ###
      legend.position = "bottom",
      legend.background = element_rect(fill = "#ffffff", 
                                       color = NA),
      
      ### Text ###
      text = element_text(color = "#22211d")
    
    )
  
  
}









  










