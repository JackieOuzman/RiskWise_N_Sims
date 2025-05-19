#More plots



library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)



Lock_water_balance_Rule <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_water_balance_rule_R_cals.csv")
str(Lock_water_balance_Rule)

# 10mm  ------------------------------------------------------------------

### keep only the spring data and results of sowing break when 10mm 

Lock_water_balance_Rule_10mm_Spring <- Lock_water_balance_Rule %>% 
  select(year, date, month, month_name, Threshold_WB_10mm) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_WB_10mm))
Lock_water_balance_Rule_10mm_Spring

### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Lock_water_balance_Rule_10mm_Spring <- Lock_water_balance_Rule_10mm_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Lock_water_balance_Rule_10mm_Spring$date <- as.POSIXct.Date(first_occurance_Lock_water_balance_Rule_10mm_Spring$date)
first_occurance_Lock_water_balance_Rule_10mm_Spring <- first_occurance_Lock_water_balance_Rule_10mm_Spring %>%  mutate(DOY = lubridate::yday(date))


# 5mm  ------------------------------------------------------------------
### keep only the spring data and results of sowing break when 4 days of rain is more than 4 days of evap

Lock_water_balance_Rule_5mm_Spring <- Lock_water_balance_Rule %>% 
  select(year, date, month, month_name, Threshold_WB_5mm) %>% 
  filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul")) %>% 
  filter(!is.na(Threshold_WB_5mm))
Lock_water_balance_Rule_5mm_Spring

### keep the first occurrence by year - for each year in the spring period when did the first sowing break occur
first_occurance_Lock_water_balance_Rule_5mm_Spring <- Lock_water_balance_Rule_5mm_Spring %>%
  group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==1) 

first_occurance_Lock_water_balance_Rule_5mm_Spring$date <- as.POSIXct.Date(first_occurance_Lock_water_balance_Rule_5mm_Spring$date)
first_occurance_Lock_water_balance_Rule_5mm_Spring <- first_occurance_Lock_water_balance_Rule_5mm_Spring %>%  mutate(DOY = lubridate::yday(date))








# 10mm and 5 mm in one file ------------------------------------------------------------------

ungroup(first_occurance_Lock_water_balance_Rule_10mm_Spring)
ungroup(first_occurance_Lock_water_balance_Rule_5mm_Spring)


names(first_occurance_Lock_water_balance_Rule_10mm_Spring)
names(first_occurance_Lock_water_balance_Rule_5mm_Spring)


first_occurance_Lock_water_balance_Rule_10mm_Spring <- first_occurance_Lock_water_balance_Rule_10mm_Spring %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing10mm") %>% 
  select(-"Threshold_WB_10mm")

first_occurance_Lock_water_balance_Rule_5mm_Spring <- first_occurance_Lock_water_balance_Rule_5mm_Spring %>% 
  mutate(Optimal_sowing_date = "Optimal_sowing5mm") %>% 
  select(-"Threshold_WB_5mm")


first_occurance_Lock_water_balance_Rule_10_5mm_Spring <- rbind(first_occurance_Lock_water_balance_Rule_10mm_Spring, 
                                                               first_occurance_Lock_water_balance_Rule_5mm_Spring)
first_occurance_Lock_water_balance_Rule_10_5mm_Spring

### PLOT
ggplot(first_occurance_Lock_water_balance_Rule_10_5mm_Spring, aes(x = DOY)) + 
  stat_ecdf(aes(color = Optimal_sowing_date),
    geom = "point") +
  theme_bw() +
  labs(
    color = NULL, #removes the legend title
    title = "Water balance rule. ",
    subtitle = "Water balance used to define optimal conditions, yesterdays water balance + todays rain - today evaporation",
    x = "Day of year ",
    y = "Probability",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured"
  )

path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
       filename = paste0(path_saved_files,"/water_balance_sowing_rule_cdf", ".png" ),
       width = 20, height = 12, units = "cm")


