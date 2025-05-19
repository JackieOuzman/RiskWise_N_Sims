library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(Lock_climate)



Lock_climate <- Lock_climate %>% 
  mutate(
    # First pass - calculate WB1 and initial WB2
    WB1 = (rain + lag(WB2, default = 0)) - evap,
    WB2 = case_when(WB1 < 1 ~ 0, .default = WB1),
    
    # Reset WB2 to zero on 2020-01-01 and calculate subsequent values
    WB2 = case_when(
      as.Date(date) == as.Date(paste0(year, "-04", "-01")) ~ 0,  # Set to zero on April 1, 2020
      as.Date(date) < as.Date(paste0(year, "-04", "-01")) ~ WB2,  # Use initial values before April 1, 2020
      TRUE ~ WB2  # Use calculated values after Jan 1, 2020
    ),
    
    # Thresholds remain the same
    Threshold_WB_5mm = case_when(WB2 > 5 ~ "Sowing_break"),
    Threshold_WB_10mm = case_when(WB2 > 10 ~ "Sowing_break")
  ) 


# Check_Lock_climate <- Lock_climate %>% select(year, date, rain, evap, WB1, WB2,Threshold_WB_5mm ) %>% 
#   filter(year == 2021)

names(Lock_climate)
Lock_climate_yr_spring <- Lock_climate %>%
   select(
     month_name,
     day_of_month,
     year,
     date,
     rain,
     evap,
     Threshold_WB_5mm,
     Threshold_WB_10mm
   ) %>%
   filter(month_name %in% c("Apr" , "May" , "Jun" , "Jul"))

#########################################################################
climate_yr_long <- Lock_climate_yr_spring %>% 
   pivot_longer(cols = rain:evap,
                names_to = "variable",
                values_to = "value"
   )
climate_yr_long


climate_yr_long <- climate_yr_long %>% 
  mutate(optimal_sowing_WB_5mm = case_when(
    Threshold_WB_5mm == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rulesWB5mm <- climate_yr_long %>%  filter(!is.na(Threshold_WB_5mm)) %>% select(date, year, optimal_sowing_WB_5mm)


sowing_rulesWB5mm_plot <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  ggplot(aes(x = date, value , color= variable))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  geom_vline(data = filter(sowing_rulesWB5mm, year %in% c(2023, 2024)), 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Water balance rule 5 mm",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "rain and evap"
  )
sowing_rulesWB5mm_plot
path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files

ggsave(plot = sowing_rulesWB5mm_plot,
       filename = paste0(path_saved_files,"/sowing_rulesWB5mm_plot2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")


#########################################################################



climate_yr_long <- climate_yr_long %>% 
  mutate(optimal_sowing_WB_10mm = case_when(
    Threshold_WB_10mm == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rulesWB10mm <- climate_yr_long %>%  filter(!is.na(Threshold_WB_10mm)) %>% select(date, year, optimal_sowing_WB_10mm)


sowing_rulesWB10mm_plot <- climate_yr_long %>% 
  filter(year %in% c(2023, 2024)) %>% 
  ggplot(aes(x = date, value , color= variable))+
  geom_line()+
  theme_bw()+
  facet_wrap(vars(year), scales = "free_x")+
  geom_vline(data = filter(sowing_rulesWB10mm, year %in% c(2023, 2024)), 
             aes(xintercept = date),  color = "grey")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
        legend.position="none"
  )+
  labs(title = "Water balance rule 10 mm",
       subtitle = "Optimal sowing rule",
       x = "",
       y = "rain and evap"
  )
sowing_rulesWB10mm_plot


ggsave(plot = sowing_rulesWB10mm_plot,
       filename = paste0(path_saved_files,"/sowing_rulesWB10mm_plot2023_2024", ".png" ),
       width = 20, height = 12, units = "cm")



path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
write_csv(Lock_climate, paste0(path_saved_files, "/Lock_water_balance_rule_R_cals.csv") )

