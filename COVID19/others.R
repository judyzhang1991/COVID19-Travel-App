## Covid 19 and Travel Visualization ##
## Jingyang Zhang ##
## May 20th, 2020 ##
## others.R ##
## This R script includes helper functions that pre-process data that will be 
## used in the visualization. 
## This file is deprecated. 






# Create a lookup hash table for looking up country of an airport
airports_hash <- hash()

for(i in 1:nrow(airports)){
  airports_hash[[airports$iata[i]]] <- airports$country[i]
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


# Helper Function: find latitude or longitude of a given country ----
## args:
## country_lat_hash: hastable of key = country, value = latitude
## country_long_hash: hashtable of key = country, value = longitude
## country_name: name of the country
## return:
## lat or long: latitude or longitude of the given country


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