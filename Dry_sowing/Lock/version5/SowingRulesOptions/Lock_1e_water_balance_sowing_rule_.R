library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate2015_2024 <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(Lock_climate2015_2024)


# year_analysis <- c("2014", "2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024")
# Lock_climate_yr <- Lock_climate2015_2024 %>% filter(year == as.double(year_analysis))

Lock_climate2015_2024

Lock_climate_yr <- Lock_climate2015_2024 %>% 
  mutate(WB = rain - evap,
         WB_recode = case_when(WB <1 ~0, .default = WB ),
         WB_1 = lag(WB_recode) + WB ,
         WB_2 = case_when(lag(WB_1)+ WB <1 ~0, .default = lag(WB_1)+ WB ),
         Threshold_WB_5mm = case_when(WB_2 > 5 ~ "Sowing_break"),
         Threshold_WB_10mm = case_when(WB_2 > 10 ~ "Sowing_break")
         )

names(Lock_climate_yr)
Lock_climate_yr_spring <- Lock_climate_yr %>%
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


