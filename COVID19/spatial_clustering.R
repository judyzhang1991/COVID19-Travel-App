library(tidyverse)
library(readr)
library(janitor)

source("map_covid.R")

# change it to per capita 


# Load data

## BORDERING COUNTRY DATA ##

borders_url <- url("https://raw.githubusercontent.com/geodatasource/country-borders/master/GEODATASOURCE-COUNTRY-BORDERS.CSV")

borders <- read_csv(borders_url) %>% clean_names()





# Helper Function: clean border information dataset ----
## args:
## dat: border information dataset
## return:
## cleaned border information dataset, with country names matching those in the covid19 dataset

clean_borderdat <- function(dat){
  
  # aland islands: not in covid19
  # american samoa: not in covid19
  # anguilla: not in covid19
  # antarctica: not in covid19
  # aruba: not in covid19
  # bermuda: not in covid19
  # bolivia (plurinational state of): bolivia in covid19
  dat$country_name[tolower(dat$country_name) == "bolivia (plurinational state of)"] <- "bolivia"
  
  dat$country_border_name[tolower(dat$country_border_name) == "bolivia (plurinational state of)"] <- "bolivia"
  
  # bonaire, sint eustatius and saba: not in covid19
  # bouvet island: not in covid19
  # british indian ocean territory: not in covid19
  # brunei darussalam: brunei in covid19
  dat$country_name[tolower(dat$country_name) == "brunei darussalam"] <- "brunei"
  dat$country_border_name[tolower(dat$country_border_name) == "brunei darussalam"] <- "brunei"
  
  # cayman islands: not in covid19
  # christmas island: not in covid19
  # cocos (keeling) islands: not in covid19
  # congo: congo (brazzaville) in covid19
  dat$country_name[tolower(dat$country_name) == "congo"] <- "congo (brazzaville)"
  
  dat$country_border_name[tolower(dat$country_border_name) == "congo"] <- "congo (brazzaville)"
  
  # congo (the democratic republic of the): congo (kinshasa) in covid19
  dat$country_name[tolower(dat$country_name) == "congo (the democratic republic of the)"] <- "congo (kinshasa)"
  
  dat$country_border_name[tolower(dat$country_border_name) == "congo (the democratic republic of the)"] <- "congo (kinshasa)"
  # cook islands: not in covid19
  # cote d’ivoire: cote d'ivoire in covid19
  dat$country_name[tolower(dat$country_name) == "cote d’ivoire"] <- "cote d'ivoire"
  
  dat$country_border_name[tolower(dat$country_border_name) == "cote d’ivoire"] <- "cote d'ivoire"
  
  # curacao: not in covid19
  # falkland islands (malvinas): not in covid19
  # faroe islands: not in covid19
  # french guiana: not in covid19
  # french polynesia: not in covid19
  # french southern territories: not in covid19
  # gambia (the): gambia in covid19
  dat$country_name[tolower(dat$country_name) == "gambia (the)"] <- "gambia"
  
  dat$country_border_name[tolower(dat$country_border_name) == "gambia (the)"] <- "gambia"
  # gibraltar: not in covid19
  # greenland: not in covid19
  # guadeloupe: not in covid19
  # guam: not in covid19
  # guernsey: not in covid19
  # heard island and mcdonald islands: not in covid19
  # hong kong: not in covid19
  # iran (islamic republic of): iran in covid19
  dat$country_name[tolower(dat$country_name) == "iran (islamic republic of)"] <- "iran"
  
  dat$country_border_name[tolower(dat$country_border_name) == "iran (islamic republic of)"] <- "iran"
  # isle of man: not in covid19
  # jersey: not in covid19
  # kiribati: not in covid19
  # korea (democratic people's republic of): korea, south in covid19
  dat$country_name[tolower(dat$country_name) == "korea (democratic people's republic of)"] <- "korea, south"
  
  dat$country_border_name[tolower(dat$country_border_name) == "korea (democratic people's republic of)"] <- "korea, south"
  # korea (the republic of): not in covid19
  # lao people's democratic republic: laos in covid19
  dat$country_name[tolower(dat$country_name) == "lao people's democratic republic"] <- "laos"
  
  dat$country_border_name[tolower(dat$country_border_name) == "lao people's democratic republic"] <- "laos"
  # macao: not in covid19
  # marshall islands: not in covid19
  # martinique: not in covid19
  # mayotte: not in covid19
  # micronesia (federated states of): not in covid19
  # moldova (the republic of): moldova in covid19
  dat$country_name[tolower(dat$country_name) == "moldova (the republic of)"] <- "moldova"
  
  dat$country_border_name[tolower(dat$country_border_name) == "moldova (the republic of)"] <- "moldova"
  # montserrat: not in covid19
  # myanmar: burma in covid19
  dat$country_name[tolower(dat$country_name) == "myanmar"] <- "burma"
  
  dat$country_border_name[tolower(dat$country_border_name) == "myanmar"] <- "burma"
  # nauru: not in covid19
  # new caledonia: not in covid19
  # niue: not in covid19
  # norfolk island: not in covid19
  # northern mariana islands: not in covid19
  # palau: not in covid19
  # palestine, state of: not in covid19
  # pitcairn: not in covid19
  # puerto rico: not in covid19
  # reunion: not in covid19
  # russian federation: russia in covid19
  dat$country_name[tolower(dat$country_name) == "russian federation"] <- "russia"
  
  dat$country_border_name[tolower(dat$country_border_name) == "russian federation"] <- "russia"
  # saint barthelemy: not in covid19
  # saint helena,\"ascension and tristan da cunha: not in covid19
  # saint martin (french part): not in covid19
  # saint pierre and miquelon: not in covid19
  # samoa:
  # sint maarten (dutch part): not in covid19
  # solomon islands: not in covid19
  # south georgia and the south sandwich islands: not in covid19
  # svalbard and jan mayen: not in covid19
  # syrian arab republic: syria in covid19
  dat$country_name[tolower(dat$country_name) == "syrian arab republic"] <- "syria"
  
  dat$country_border_name[tolower(dat$country_border_name) == "syrian arab republic"] <- "syria"
  # taiwan (province of china): not in covid19
  # tanzania (the united republic of): tanzania in covid19
  dat$country_name[tolower(dat$country_name) == "tanzania (the united republic of)"] <- "tanzania"
  
  dat$country_border_name[tolower(dat$country_border_name) == "tanzania (the united republic of)"] <- "tanzania"
  # tokelau: not in covid19
  # tonga: not in covid19
  # turkmenistan: not in covid19
  # turks and caicos islands: not in covid19
  # tuvalu: not in covid19
  # united kingdom of great britain and northern ireland: not in covid19
  # united states minor outlying islands: not in covid19
  # united states of america: us in covid19
  dat$country_name[tolower(dat$country_name) == "united states of america"] <- "us"
  
  dat$country_border_name[tolower(dat$country_border_name) == "united states of america"] <- "us"
  # vanuatu: not in covid19
  # venezuela (bolivarian republic of): venezuela in covid19
  dat$country_name[tolower(dat$country_name) == "venezuela (bolivarian republic of)"] <- "venezuela"
  
  dat$country_border_name[tolower(dat$country_border_name) == "venezuela (bolivarian republic of)"] <- "venezuela"
  # viet nam: vietname in covid19
  dat$country_name[tolower(dat$country_name) == "viet nam"] <- "vietnam"
  
  dat$country_border_name[tolower(dat$country_border_name) == "viet nam"] <- "vietnam"
  # virgin islands (british): not in covid19
  # virgin islands (u.s.): not in covid19
  # wallis and futuna: not in covid19
  
  return(dat)
}


borders <- clean_borderdat(borders)


## COVID19 DATA ##

date_str <- "05-22-2020"

covid19 <- getDat(date_str) %>% clean_coviddat()




## Helper Function: get_borders
## Gets border countries of a given country
## Arg: 
## borders: dataset containing border information
## country: country of interest
## Return: list of countries that share border with the given country

get_borders <- function(borders, country){
  border_countries <- borders %>% filter(tolower(country_name) == tolower(country))
  
  border_countries <- tolower(border_countries$country_border_name)
  
  return(border_countries)
}


## Function: get_connect
## Creates a connect matrix using information from the borders dataset
## Arg: 
## borders: dataset containing border information
## Return: a connectivity matrix 


get_connect <- function(borders){
  # Get list of unique countries 
  countries <- tolower(unique(borders$country_name))
  # Create an empty connect matrix
  connect_matrx <- matrix(data = NA, nrow = length(countries), ncol = length(countries))
  colnames(connect_matrx) <- countries
  rownames(connect_matrx) <- countries
  
  # Get border information of every country in the countries list
  for(i in 1 : length(countries)){
    country <- countries[i]
    border_countries <- get_borders(borders, country)
    
    if(is.na(border_countries)){
      next
    }
    else{
      
      if(length(border_countries) > 1){
        for(j in 1 : length(border_countries)){
          
          connect_matrx[border_countries[j], country] <- 1
        }
      }
      else{
        
        connect_matrx[border_countries, country] <- 1
      }
      
    }
    
  }
  
  # Replace all NA with 0s
  connect_matrx[is.na(connect_matrx)] <- 0
  
  return(connect_matrx)
  
  
}



# Get connectivity matrix
connect_matrx <- get_connect(borders)

## Function: value_lookup
## Look up value of confirmed/recovered/deaths cases of a given country
## Arg: 
## country: country of interest
## covid19: covid19 dataset
## type: which type of value to draw, confirmed, recovered, deaths
## Return: a number of value of confirmed/recovered/deaths cases of the given county

value_lookup <- function(covid19, country, type){
  country_value <- covid19 %>% 
    filter(tolower(country_region) == tolower(country))
  
  case_value <- case_when(
    type == "confirmed" ~ country_value$total_confirmed,
    type == "recovered" ~ country_value$total_recovered,
    type == "deaths" ~ country_value$total_deaths
  )
  
  if(length(case_value) == 0){
    case_value <- 0
  }
  
  return(case_value)
  
}




## Helper Function: cal_moransI
## Calculate Moran's I test statistics of a given date
## Arg: 
## borders: border information dataset
## covid19: covid19 dataset
## type: which type of value to draw, confirmed, recovered, deaths
## Return: calculted Moran's I statistic and corresponding p-value

cal_moransI <- function(borders, covid19, type){
  attr_vals <- case_when(
    type == "confirmed" ~ covid19$total_confirmed,
    type == "recovered" ~ covid19$total_recovered,
    type == "deaths" ~ covid19$total_deaths
  )
  
  var_attr <- var(attr_vals)
  mean_attr <- mean(attr_vals)
  n <- length(attr_vals)
  
  
  # Get connectivity matrix
  connect_matrx <- get_connect(borders)
  
  sum_weights <- sum(connect_matrx)
  
  # Calculate Moran's I
  weighted <- c()
  for(i in 1 : nrow(connect_matrx)){
    for(j in 1: nrow(connect_matrx)){
      si <- rownames(connect_matrx)[i]
      sj <- rownames(connect_matrx)[j]
      zsi <- value_lookup(covid19, si, type)
      zsj <- value_lookup(covid19, sj, type)
      
      value <- connect_matrx[i, j] * (zsi - mean_attr) * (zsj - mean_attr)
      
      weighted[i] <- value
      
    }
  }
  
  I <- (n / ((n - 1) * var_attr * sum_weights)) * sum(weighted)
  
  return(I)
}


cal_moransI(borders, covid19, "confirmed")




moransI <- c()


dates <- format(seq(as.Date("2020/1/22"), as.Date(Sys.Date() - 1), by = "day"), "%m-%d-%Y")


for(i in 1 : length(dates)){
  covid19 <- getDat(dates[i]) %>% clean_coviddat()
  I <- cal_moransI(borders, covid19, "confirmed")
  moransI[i] <- I
}
