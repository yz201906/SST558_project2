library(rmarkdown)
library(tidyverse)

days <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

map(.x = days, .f = ~render(input = "README.Rmd", 
                            output_file = paste0(.x,"report.html"), 
                            params = list(day_of_week = .x))
    )
