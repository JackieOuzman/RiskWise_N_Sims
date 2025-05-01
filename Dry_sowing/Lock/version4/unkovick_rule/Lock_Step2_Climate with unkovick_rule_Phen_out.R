# This file is for importing met file assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)





# Download Daily climate files created in step 1 (with frost days) -------------------------------------------------------

met_frost <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Fost_details_18046.csv")
str(met_frost)




# Download Phenlogy next gen files  -----------------------------------------------


Dry_sowing_Lock_factor <- read_excel("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version4_unkovich_rule/unkovich_factor_phen_dates_v4.xlsx", 
                                     sheet = "SowingPhenology", col_types = c("text", 
                                                                              "numeric", "text", "numeric", "text", 
                                                                              "text", "text", "text", "date", "numeric", 
                                                                              "text", "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric"))
  
  
# Dry_sowing_Lock_factor <-Dry_sowing_Lock_factor %>% 
#   mutate(year = year(Clock.Today))
##something is wrong with PAW sum
# test2024 <- Dry_sowing_Lock_factor %>% filter(year == 2024) %>% 
#   filter(StartDate == '10-may')
# 
# test2024 <- test2024 %>% 
# mutate(PAW_Sum = (`Soil.Water.PAW(1)`+                         
#                      `Soil.Water.PAW(2)`+
#                      `Soil.Water.PAW(3)`+                         
#                      `Soil.Water.PAW(4)`+
#                      `Soil.Water.PAW(5)`+                        
#                      `Soil.Water.PAW(6)`+
#                      `Soil.Water.PAW(7)`)) %>% 
#   rename(Sowing_date = StartDate)
# names(test2024)
# 
# test2024<- test2024 %>% select(year, Sowing_date, "Clock.Today",Wheat.Phenology.CurrentStageName,
#                                PAW_Sum  )
# 
# 
# test2024

str(Dry_sowing_Lock_factor)
names(Dry_sowing_Lock_factor)
Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor %>% select(
  StartDate,  #(Sowing_date)
  EndDate,
  Clock.Today ,
  Wheat.Phenology.Zadok.Stage,
  Wheat.Phenology.CurrentStageName,
  Wheat.Phenology.Stage,   
  Yield ,
  Yield_Adj_t_ha ,
  `Soil.Water.PAW(1)`,                         
  `Soil.Water.PAW(2)`,
  `Soil.Water.PAW(3)`,                         
  `Soil.Water.PAW(4)`,
  `Soil.Water.PAW(5)`,                         
  `Soil.Water.PAW(6)`,
  `Soil.Water.PAW(7)`
)

# only keep sim with the 30-may end date
Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor %>% filter(EndDate == "30-may")



## sum the PAW and change the name of sowing date
Dry_sowing_Lock_factor <-Dry_sowing_Lock_factor %>% 
  mutate(PAW_Sum = (`Soil.Water.PAW(1)`+                         
                                            `Soil.Water.PAW(2)`+
                                            `Soil.Water.PAW(3)`+                         
                                            `Soil.Water.PAW(4)`+
                                            `Soil.Water.PAW(5)`+                        
                                            `Soil.Water.PAW(6)`+
                                            `Soil.Water.PAW(7)`)) %>% 
           rename(Sowing_date = StartDate)

 test <-Dry_sowing_Lock_factor %>% 
   mutate(year = year(Clock.Today))

names(Dry_sowing_Lock_factor)
test <- test %>% 
  group_by(Sowing_date, year, Wheat.Phenology.CurrentStageName) %>% 
  summarise(PAW_Sum_mean = mean( PAW_Sum, na.rm = TRUE)
            )

test
### Make a new clm with Sensitive period of frost = 6.49 - 9.5  ------------------
summary(Dry_sowing_Lock_factor)
str(Dry_sowing_Lock_factor)

Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor %>% 
  mutate(frost_Sensitive_period = 
           case_when(
             between(Wheat.Phenology.Stage, 6.49, 9.5) ~ "frost_sensitive_period",
             .default = "outside_Sensitive_period"
           ))
### Make a new Most sensitive period 8-9.5  -----------------------------------------------
Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor %>% 
  mutate(frost_most_Sensitive_period = 
           case_when(
             between(Wheat.Phenology.Stage, 8.0, 9.5) ~ "frost_most_sensitive_period",
             .default = "outside_period"
           ))

#check <- Dry_sowing_Lock_factor %>%  filter(between(Wheat.Phenology.Zadok.Stage, 6.49, 9.5))


## Append met_frost ------------------------------------------------------------
str(Dry_sowing_Lock_factor)
str(met_frost)
Dry_sowing_Lock_factor_with_met <- left_join(Dry_sowing_Lock_factor,met_frost, by = join_by("Clock.Today" == "date"))





# plot /summaries results - number of frost days in sensitive period etc ----


## Create a new clm which defines the season and only keep data in GS ----
unique(Dry_sowing_Lock_factor_with_met$Sowing_date)

unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)
## something weird happens and sowing date is not recorded I need to add it in.

Dry_sowing_Lock_factor_with_met<- Dry_sowing_Lock_factor_with_met %>% 
  mutate(Wheat.Phenology.CurrentStageName = 
           case_when(
             is.na(Wheat.Phenology.CurrentStageName)  ~ "Sowing",
    .default =  Wheat.Phenology.CurrentStageName))
 
## add a clm with two dates marked as start and end
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% mutate(
    start_end_GS = case_when(
      Wheat.Phenology.CurrentStageName == "Sowing"  ~ "start_gs",
      Wheat.Phenology.CurrentStageName == "HarvestRipe"  ~ "end_gs",
      TRUE ~ NA))
### fill the blanks
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% fill(start_end_GS) %>%
    mutate(
      season = case_when(
        start_end_GS == "start_gs" ~ "gs",
        TRUE ~ "other"))
## not quite right here I still need to keep   "HarvestRipe"
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% mutate(
  season = case_when(
    Wheat.Phenology.CurrentStageName == "HarvestRipe"  ~ "gs",
    TRUE ~ season))


Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% select(-start_end_GS)  
str(Dry_sowing_Lock_factor_with_met)

## Only keep data in the season
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% filter(season == "gs")

## new clm when frost_Sensitive_period and frost----
unique(Dry_sowing_Lock_factor_with_met$frost_event)
unique(Dry_sowing_Lock_factor_with_met$frost_Sensitive_period)
unique(Dry_sowing_Lock_factor_with_met$frost_most_Sensitive_period)    







unique(Dry_sowing_Lock_factor_with_met$Sowing_date)

Dry_sowing_Lock_factor_with_met$Sowing_date <- factor(Dry_sowing_Lock_factor_with_met$Sowing_date , ordered = TRUE, 
                             levels = c(
                               "1-apr" ,
                               "5-apr" ,
                               "10-apr" ,
                               "15-apr" ,
                               "20-apr",
                               "25-apr" ,
                              
                               "1-may" ,
                               "5-may" ,
                               "10-may" ,
                               "15-may" ,
                               "20-may",
                               "25-may"
                              
                             ))








## Save files ----


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")
path_saved_files

write_csv(Dry_sowing_Lock_factor_with_met,
          file = paste0(path_saved_files,"/Summary_output_phenology_change", ".csv"))

