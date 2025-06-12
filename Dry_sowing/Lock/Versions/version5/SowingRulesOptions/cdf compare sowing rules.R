## merge the cdf input data for differet rule and plot

library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_cfd_rainfall_evap <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_rain_Ev_rule_R_cdf_inputs.csv")
Lock_cfd_Sum_rain <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_sum_rain_rule_R_cdf_inputs.csv")
Lock_cfd_water_balance <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Lock_water_balance_rule_R_cdf_inputs.csv")


first_occurance <- rbind(Lock_cfd_rainfall_evap, Lock_cfd_Sum_rain, Lock_cfd_water_balance)
names(first_occurance)
unique(first_occurance$Optimal_sowing_date)

first_occurance <- first_occurance %>% 
  mutate(sowing_rule = case_when(
    Optimal_sowing_date == "Optimal_sowing3day15mm" ~ "Sum rain 3days 15mm",
    
    Optimal_sowing_date == "Optimal_sowing2days" ~ "Rain exceeds evaporation 2days",
    Optimal_sowing_date == "Optimal_sowing4days" ~ "Rain exceeds evaporation 4days",
    Optimal_sowing_date == "Optimal_sowing7days" ~ "Rain exceeds evaporation 7days",
    
    Optimal_sowing_date == "Optimal_sowing10mm" ~ "Water balance rule thershold 10mm",
    Optimal_sowing_date == "Optimal_sowing5mm" ~ "Water balance rule thershold 5mm"
    
  ))


### PLOT 
# NB plotting the day and month is a pain
# The approach I've used for this is to create a date for plotting purposes that is in a single year 
#(I usually pick a leap year to accommodate 2/29 data should it arise)

str(first_occurance)
first_occurance$CommonDate <- ymd(paste0("2000-",str_sub(as.character(first_occurance$date),-5)))

ggplot(first_occurance, 
       #aes(x = yday(date))) +
       aes(x = CommonDate)) + #as defined above
  stat_ecdf(aes(color = sowing_rule),
            geom = "point") +
  theme_bw() +
 

  labs(
    color = NULL, #removes the legend title
    title = "Sowing rules",
    subtitle = "Different rules used to define optimal conditions",
    x = "",
    y = "Probability",
    caption = "Data is: DOY per year that first occurance of optimal conditions has occured"
  )



path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(
  filename = paste0(path_saved_files,"/compare_sowing_rule_cdf", ".png" ),
  width = 20, height = 12, units = "cm")

