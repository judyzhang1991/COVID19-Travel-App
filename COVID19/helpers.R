## STAT302 Data Visualization
##
## Final Project
##
## Jingyang Zhang
##
## May 22, 2020



### MAP DATA ###

map_dat <- maps::map(
  database = "world",
  plot = FALSE,
  fill = TRUE
)


# Cutoffs based on the number of cases
val_breaks <- c(1, 20 , 100, 1000, 50000)


# Helper Function: get country outlines

# Get outline data for a given state
get_country_outline <- function(){
  states_outline <- maps::map(
    database = "state",
    plot = FALSE,
    fill = TRUE
  ) %>%
    st_as_sf() 
  
  if(grepl(",", state_input, fixed = TRUE)){
    return(states_outline)
  }
  else{
    states_outline <- subset(states_outline, 
                             ID == tolower(state_input))
    return(states_outline)
  }
  
  
}




# Helper Function: map covid 19 data ----
## args:
## dat: dataset to map (confirmed cases, deaths cases, recovered cases)
## color: color of the data points (confirmed, deaths, recovered)
## size: size of the data points (confirmed, deaths, recovered)
## legend_title: title of the legend
## start: starting date
## end: ending date

covid_map <- function(dat, legend_title, date) {
  
  # Manipulate input date to get corresponding column

  ymd <- str_split(gsub(" 0", " ", format(as.Date(date), "%Y, %m, %d")), ", ", simplify = TRUE)

  y <- substr(ymd[,1], 3, 4)
  
  m <- ymd[,2]
  
  d <- ymd[,3]
  
  date_col <- paste("x", paste(paste(m, d, sep = "_"), y, sep = "_"), sep = "")
  print(nrow(dat))
  
  ggplot() + 
    geom_polygon(data = map_dat, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = "grey",
                 alpha = 0.3) + 
    
    geom_point(data = dat, 
               aes(x = longitude, 
                   y = latitude, 
                   color = dat[[date_col]]), 
               size = 5,
               stroke = F,
               alpha = 0.7) + 
    #scale_size_continuous(name=legend_title, 
                      #    trans="log", 
                      #    range=c(1,7),
                      #   breaks=val_breaks, 
                      #    labels = c("1-19", 
                                  #   "20-99", 
                                  #   "100-999", 
                                  #  "1,000-49,999", 
                                  #  "50,000+")) +
   
    scale_color_viridis_c(option="inferno",
                          name=legend_title, 
                          trans="log",
                          breaks=val_breaks, 
                          labels = c("1-19", 
                                     "20-99", 
                                     "100-999", 
                                     "1,000-49,999", 
                                     "50,000+")) +
    theme_void() + 
    guides( colour = guide_legend()) +
    labs(caption = "Data Repository provided by Johns Hopkins CSSE.") +
    theme(
      legend.position = "bottom",
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#ffffff", color = NA), 
      panel.background = element_rect(fill = "#ffffff", color = NA), 
      legend.background = element_rect(fill = "#ffffff", color = NA)
    )
    
}
