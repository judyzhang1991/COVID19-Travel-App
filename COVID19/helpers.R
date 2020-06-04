## STAT302 ##
## Final Project ##
## Jingyang Zhang ##
## May 20th, 2020 ##
## helpers.R ##


# Cutoffs based on the number of cases
val_breaks <- c(1, 20 , 100, 1000, 50000)


# Helper Function: clean covid data, group by country region and make it tidy ----
## args:
## dat: dataset (confirmed cases, deaths cases, recovered cases)
## return:
## dat: cleaned covid data set

clean_coviddat <- function(dat){
  dat <- dat %>% 
    clean_names() %>%
    group_by(country_region) %>%
    summarise_at(vars(starts_with('x')), sum) %>%
    pivot_longer(cols = -country_region,
                 names_to = "date",
                 values_to = "cases") %>%
    # Remove "x" from date, change date format to %d-%m-%y
    mutate(
      date = as.Date(str_replace_all(str_replace(date, "x", ""), "_", "-"), "%m-%d-%y"),
      country_region = tolower(country_region),
      categ = case_when(
        cases >= 0 & cases <= 19 ~ "0-19",
        cases >= 20 & cases <= 99 ~ "20-99",
        cases >= 100 & cases <= 999 ~ "100-999",
        cases >= 1000 & cases <= 49999 ~ "1000-49,999",
        cases >= 50000 ~ "50,000+"
      )
    )
}





# Helper Function: map covid 19 data ----
## args:
## dat: dataset to map (confirmed cases, deaths cases, recovered cases)
## date_input: which date of data to display
## legend_title: title of the legend
## color: which set of colors to use for fill

covid_map <- function(dat, date_input, legend_title, color) {
      
        
      
      ## Filter date to find the given date
      plot_dat <- dat[dat$date == date_input, ]
      
      plot_dat$categ = factor(plot_dat$categ, 
                              levels = c("0-19",
                                         "20-99",
                                         "100-999",
                                         "1000-49,999",
                                         "50,000+"))

      ggplot(plot_dat) +
        
        geom_sf(aes(fill = categ, 
                    geometry = geom.y),
                color = "darkgrey",
                show.legend = "polygon") + 
        
        scale_fill_manual(name = legend_title,
                          values = color,
                          na.value = "#EEEEEE") +
        theme_minimal() + 
        
        labs(caption = "Data Repository provided by Johns Hopkins CSSE.") +
        
        theme(
          ### Plot ###
          plot.background = element_rect(fill = "#ffffff", 
                                         color = NA),
          
          ### Panel ###
          panel.background = element_rect(fill = "#ffffff", 
                                          color = NA),
          
          panel.grid = element_blank(),
          
          ### Axis ###
          axis.text = element_blank(),
          
          ### Legend ###
          legend.position = "bottom",
          
          legend.background = element_rect(fill = "#ffffff", 
                                           color = NA),
          
          ### Text ###
          text = element_text(color = "#22211d"),
           
        )
        
        
}







# Helper Function: barplot for the top 10 countries with most cases ----
## args:
## dat: dataset to map (confirmed cases, deaths cases, recovered cases)
## title: title of the plot
## date_input: which date of data to display
## color: which set of colors to use for fill

top_ten <- function(dat, title_input, date_input, color){
  ## Filter date to find the given date
  ## also get the top ten countries with most cases
  plot_dat <- dat[dat$date == date_input, ] %>%
    filter(cases > 0) %>%
    arrange(desc(cases))
  
  if(nrow(plot_dat) < 10){
    top_countries = plot_dat
  }else{
    top_countries = plot_dat[1:10,]
  }
  
  
  plot_title = paste("Top 10 Countries with Most ", title_input, sep = "")

  
  ggplot(top_countries, aes(x = reorder(ID.x, cases), y = cases)) + 
    
    geom_bar(aes(fill = categ), 
             stat = "identity", 
             show.legend = FALSE) + 
    
    geom_text(aes(label = cases), 
              position = position_dodge(width = 0.9),
              hjust = 1,
              color = "#d1c0df",
              size = 5,
              fontface = "bold") + 
    
    scale_fill_manual(values = color) + 
    
    coord_flip() +
    
    labs(title = plot_title, 
         x = "Country", 
         y = "Number of Cases",
         caption = "Only display countries with at least 1 case.") + 
    
    theme_minimal() + 
    
    theme(
      
      ### Plot ###
      plot.background = element_blank(),
      
      plot.title = element_text(face = "bold",
                                size = 24,
                                hjust = 0,
                                vjust = 1),
      
      ### Panel ###
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      
      ### Axis ###
      
      axis.text = element_text(face = "bold", 
                               size = 10),
      
      axis.title = element_text(face = "bold",
                                size = 12)
      
     
      
    )
    
  
    
  
  
}

