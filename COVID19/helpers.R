## STAT302 Data Visualization
##
## Final Project
##
## Jingyang Zhang
##
## May 22, 2020



# Helper Function: covid19_map ----
## args:
## dat: dataset to map (confirmed cases, deaths cases, recovered cases)
## color: color of the data points (confirmed, deaths, recovered)
## size: size of the data points (confirmed, deaths, recovered)
## legend_title: title of the legend
## start: starting date
## end: ending date

covid_map <- function(dat, legend_title) {

  ggplot() + 
    geom_polygon(data = map_dat, 
                 aes(x = long, 
                     y = lat, 
                     group = group),
                 fill = "grey",
                 alpha = 0.3) + 
    
    geom_point(data = dat, 
               aes(x = long, 
                   y = lat, 
                   #size = x3_3_20, 
                   color = x3_3_20), 
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
