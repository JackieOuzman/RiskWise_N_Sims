#More plots



library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)



Lock_sum_rain_Rule <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_sum_rain_3days_15mm_rule_R_cals.csv")
str(Lock_sum_rain_Rule)

# 15mm 3 days  ------------------------------------------------------------------

### keep only the spring data and results of sowing break 

Lock_sum_rain_Rule_Spring <- Lock_sum_rain_Rule %>% 
  select(year, date, month, month_name, Threshold_3day_15) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_3day_15))
Lock_sum_rain_Rule_Spring

### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Lock_sum_rain_Rule_Spring <- Lock_sum_rain_Rule_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Lock_sum_rain_Rule_Spring$date <- as.POSIXct.Date(first_occurance_Lock_sum_rain_Rule_Spring$date)
first_occurance_Lock_sum_rain_Rule_Spring <- first_occurance_Lock_sum_rain_Rule_Spring %>%  mutate(DOY = lubridate::yday(date))





ungroup(first_occurance_Lock_sum_rain_Rule_Spring)

names(first_occurance_Lock_sum_rain_Rule_Spring)


first_occurance_Lock_sum_rain_Rule_Spring <- first_occurance_Lock_sum_rain_Rule_Spring %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing3day15mm") %>% 
  select(-"Threshold_3day_15")




### PLOT
ggplot(first_occurance_Lock_sum_rain_Rule_Spring, aes(x = DOY)) + 
  stat_ecdf(aes(color = Optimal_sowing_date),
    geom = "point") +
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Water balance rule. ",
    subtitle = "Sum of rain used to define optimal conditions, sum last 3 days rain threshold 15mm",
    x = "Day of year ",
    y = "Probability",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured"
  )

path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
       filename = paste0(path_saved_files,"/sum_rain3days15mm_sowing_rule_cdf", ".png" ),
       width = 20, height = 12, units = "cm")




###  cdf input for plots
path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
write_csv(first_occurance_Lock_sum_rain_Rule_Spring, 
          paste0(path_saved_files, "/Lock_sum_rain_rule_R_cdf_inputs.csv") )



