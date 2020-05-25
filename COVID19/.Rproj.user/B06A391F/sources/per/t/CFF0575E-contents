## STAT302 Data Visualization
##
## Final Project
##
## Jingyang Zhang
##
## May 22, 2020



# Helper Function: covid19_map ----
## args: 
## var: mapping variable
## color: fill for the map
## legend.title: title of the legend
## start: starting date
## end: ending date
covid_map <- function(dat, var, color,legend_title, display_date) {

  # Filter data by date
  plot_dat <- dat %>% filter(date == display_date)
  
 
  # Get mapping variable
  map_var <- switch(var,
                    "confirmed" = plot_dat$confirmed_rate,
                    "recovered" = plot_dat$recovered_rate,
                    "deaths" = plot_dat$deaths_rate)
 

  # plot choropleth map
  ggplot(plot_dat) + 
    geom_sf(aes(fill = map_var, 
                geometry = geom), 
            color = "grey25") +
    
    scale_fill_gradientn(name = legend_title, 
                         colors = brewer.pal(5, color),
                         na.value = "#762A83") + # Purple color for NAs
    theme_minimal() + 
    
    theme(
      
      ### Plot ###
      plot.background = element_blank(),
      
      ### Panel ###
      panel.grid = element_blank(),
      
      
      ### Axis ###
      axis.text = element_blank(),
      
      
      ### Legend ###
      legend.direction = "horizontal",
      
      legend.position = "bottom"
    )


 
}
