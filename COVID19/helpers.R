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
      date = str_replace_all(str_replace(date, "x", ""), "_", "-"),
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
## color: color of the data points (confirmed, deaths, recovered)
## size: size of the data points (confirmed, deaths, recovered)
## legend_title: title of the legend
## start: starting date
## end: ending date

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
                    geometry = geom),
                color = "white",
                show.legend = "polygon") + 
        
        scale_fill_manual(name = legend_title,
                          values = color) +
        theme_minimal() + 
        
        labs(caption = "Data Repository provided by Johns Hopkins CSSE.") +
        
        theme(
          ### Plot ###
          plot.background = element_rect(fill = "#ffffff", color = NA),
          
          ### Panel ###
          panel.background = element_rect(fill = "#ffffff", color = NA),
          
          panel.grid = element_blank(),
          
          ### Axis ###
          axis.text = element_blank(),
          
          ### Legend ###
          legend.position = "bottom",
          
          legend.background = element_rect(fill = "#ffffff", color = NA),
          
          ### Text ###
          text = element_text(color = "#22211d"),
           
        )
        
        
}




