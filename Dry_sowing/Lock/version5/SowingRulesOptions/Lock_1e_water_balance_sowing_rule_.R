library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)


Lock_climate <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(Lock_climate)




  # Create copy of input data
  df <- Lock_climate
  
  # Initialize variables to zero
  df <- df %>%
    mutate(
      WB1 = 0,
      WB2 = 0,
      previousDayWB2 = 0 #,
     
    )
  
  # Determine reset days (April 1st)
  df <- df %>%
    mutate(isResetDay = format(date, "%m-%d") == "04-01")
  
  # Process each day sequentially ### this is a bit hairy for me - but it works!
  for (i in 1:nrow(df)) {
    # Get today's values
    today <- df[i, ]
    todaysRain <- today$rain
    todaysEvap <- today$evap
    isResetDay <- today$isResetDay
    
    if (i == 1) {
      # First day of df set everything to zero
      df$WB2[i] <- 0
      df$previousDayWB2[i] <- 0
    } else {
      # Get previous day's WB2
      if (isResetDay) {
        # On reset day (April 1st), set previous day WB2 to 0
        previousDayWB2 <- 0
      } else {
        # Otherwise use the previous day's calculated WB2
        previousDayWB2 <- df$WB2[i-1]
      }
      df$previousDayWB2[i] <- previousDayWB2
      
      # Calculate WB1: min of 20 and (rain + previous WB2 - evap)
      WB1 <- ifelse(
        ((todaysRain + previousDayWB2) - todaysEvap) > 20,
        20,
        ((todaysRain + previousDayWB2) - todaysEvap)
      )
      df$WB1[i] <- WB1
      
      # Calculate WB2: WB1 if >= 0, otherwise 0
      WB2 <- ifelse(WB1 < 0, 0, WB1)
      
      # If today is April 1st, reset WB2 to 0 after calculation
      if (isResetDay) {
        WB2 <- 0
      }
      
      df$WB2[i] <- WB2
          }
  }
  
  # Determine sowing conditions
  df <- df %>%
    mutate(
     
      Threshold_WB_5mm = ifelse(WB2 > 5, "Sowing_break", NA_character_),
      Threshold_WB_10mm = ifelse(WB2 > 10, "Sowing_break", NA_character_)
    )
  
  





 Check_Lock_climate <- df %>% select(year, date, rain, evap, WB1, WB2,Threshold_WB_5mm ) %>% 
   filter(year == 2020)

names(Lock_climate)
df_spring <- df %>%
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
df_spring_long <- df_spring %>% 
   pivot_longer(cols = rain:evap,
                names_to = "variable",
                values_to = "value"
   )
df_spring_long


df_spring_long <- df_spring_long %>% 
  mutate(optimal_sowing_WB_5mm = case_when(
    Threshold_WB_5mm == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rulesWB5mm <- df_spring_long %>%  filter(!is.na(Threshold_WB_5mm)) %>% select(date, year, optimal_sowing_WB_5mm)


sowing_rulesWB5mm_plot <- df_spring_long %>% 
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



df_spring_long <- df_spring_long %>% 
  mutate(optimal_sowing_WB_10mm = case_when(
    Threshold_WB_10mm == "Sowing_break" ~ 40,
    .default = 0
  ))
sowing_rulesWB10mm <- df_spring_long %>%  filter(!is.na(Threshold_WB_10mm)) %>% select(date, year, optimal_sowing_WB_10mm)


sowing_rulesWB10mm_plot <- df_spring_long %>% 
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

