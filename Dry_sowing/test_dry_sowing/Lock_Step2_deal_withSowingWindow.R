## This is an extra step to deal with permeations created when factorising a sowing rule with a window
# this needs to match "Lock_Step2_climate_fiexed_sowing" output

library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

# Download Daily climate files created in step 1 (with frost days) -------------------------------------------------------

met_frost <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Fost_details_18046.csv")
str(met_frost)


Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Dry_sowing_Lock_factor_KPV2 Small_2022_2024.Daily.csv", 
                                              col_types = cols(Clock.Today = col_character()))
names(Dry_sowing_Lock_factor_KPV2_Daily_2022_2024)
str(Dry_sowing_Lock_factor_KPV2_Daily_2022_2024)


Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 %>% select(
  CheckpointName,
  SimulationName,
  StartDate,
  EndDate,
  #Sowing_date,
  Clock.Today ,
  Wheat.Phenology.Zadok.Stage,
  Wheat.Phenology.CurrentStageName,
  Yield ,
  Yield_Adj_t_ha ,
  Wheat.Phenology.Stage ,
  `Soil.Water.PAW(1)`  , 
  `Soil.Water.PAW(2)`,                         
  `Soil.Water.PAW(3)` ,  
  `Soil.Water.PAW(4)`,                         
  `Soil.Water.PAW(5)` , 
  `Soil.Water.PAW(6)`,                         
  `Soil.Water.PAW(7)` 
)

## fix up some names
Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- 
  Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 %>%
  rename(
    PAW_1 =`Soil.Water.PAW(1)`  , 
    PAW_2 =`Soil.Water.PAW(2)`,                         
    PAW_3 =`Soil.Water.PAW(3)` ,  
    PAW_4 =`Soil.Water.PAW(4)`,                         
    PAW_5 =`Soil.Water.PAW(5)` , 
    PAW_6 =`Soil.Water.PAW(6)`,                         
    PAW_7 =`Soil.Water.PAW(7)` ) %>% 
  mutate(PAW_SUM = PAW_1+ PAW_2+PAW_3+PAW_4+PAW_5+PAW_6+PAW_7)


## Make a 2 new clm called 'Sowing_date' that will match the other files and a temp file to work out what to keep

Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 %>% 
  mutate(Sowing_window = paste0(StartDate, "-",EndDate ))
unique(Dry_sowing_Lock_factor_KPV2_Daily_2022_2024$Sowing_window)
# what I want to keep is:
sowing_window_retain <- c("1-apr-5-apr",
                          "5-apr-10-apr",
                          "10-apr-15-apr",
                          "15-apr-20-apr",
                          "20-apr-25-apr",
                          "25-apr-30-apr",
                          
                          "1-may-5-may",
                          "5-may-10-may",
                          "10-may-15-may",
                          "15-may-20-may",
                          "20-may-25-may",
                          "25-may-30-may")
                          
Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 %>% 
  filter(Sowing_window %in% sowing_window_retain)
unique(Dry_sowing_Lock_factor_KPV2_Daily_2022_2024$Sowing_window)

Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 <- Dry_sowing_Lock_factor_KPV2_Daily_2022_2024 %>% 
  rename(Sowing_date = StartDate)
names(Dry_sowing_Lock_factor_KPV2_Daily_2022_2024)

## Now we can use the template / structure from Step2 
Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor_KPV2_Daily_2022_2024

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

Dry_sowing_Lock_factor$Clock.Today <- as.Date(Dry_sowing_Lock_factor$Clock.Today)
Dry_sowing_Lock_factor_with_met <- left_join(Dry_sowing_Lock_factor,met_frost, by = join_by("Clock.Today" == "date"))





# plot /summaries results - number of frost days in sensitive period etc ----


## Create a new clm which defines the season and only keep data in GS ----
unique(Dry_sowing_Lock_factor_with_met$Sowing_date)

#testing process with subset of data
# test_1_apri_2000 <- Dry_sowing_Lock_factor_with_met %>% 
#   filter(Sowing_date == "1-apr") %>% 
#   filter(year == 2000)
# str(test_1_apri_2000)
# unique(Dry_sowing_Lock_factor_with_met$Wheat.Phenology.CurrentStageName)



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





## new clm when a frost event occur is sensitive period
Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% 
  mutate(
    frost_in_Sensitive_period = case_when(
      frost_event == "frost" | frost_Sensitive_period == "frost_most_sensitive_period"~ "frost_in_Sensitive_period",
      TRUE ~ "no_frost_Sensitive_period"))


Dry_sowing_Lock_factor_with_met <- Dry_sowing_Lock_factor_with_met %>% 
  mutate(
    frost_in_most_Sensitive_period = case_when(
      frost_event == "frost" | 
        frost_most_Sensitive_period == "frost_most_Sensitive_period"~ "frost_in_most_Sensitive_period",
      TRUE ~ "no_frost_most_Sensitive_period"))

#test
str(Dry_sowing_Lock_factor_with_met)
test <- Dry_sowing_Lock_factor_with_met %>% filter(year == 2024) %>%  filter(Sowing_date== "10-may")


# Save file
write.csv(Dry_sowing_Lock_factor_with_met ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Dry_sowing_Lock_factor_with_met_180462022_2024.csv", row.names = FALSE )


## Summaries data how many frost days in GS by year----
names(Dry_sowing_Lock_factor_with_met)
str(Dry_sowing_Lock_factor_with_met)

days_GS <- Dry_sowing_Lock_factor_with_met %>% 
  group_by(year,Sowing_date ) %>% 
  count(season)
days_GS <- days_GS %>%  rename(days_GS = n)
days_GS

frost_event_count_year <- Dry_sowing_Lock_factor_with_met %>% 
  group_by(year,Sowing_date) %>% 
  count(frost_event)

frost_event_count_year <- frost_event_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_event)
frost_event_count_year


frost_in_Sensitive_period_count_year <- Dry_sowing_Lock_factor_with_met %>% 
  group_by(year,Sowing_date) %>% 
  count(frost_in_Sensitive_period)
frost_in_Sensitive_period_count_year <- frost_in_Sensitive_period_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_in_Sensitive_period)
frost_in_Sensitive_period_count_year

frost_in_most_Sensitive_period_count_year <- Dry_sowing_Lock_factor_with_met %>% 
  group_by(year,Sowing_date) %>% 
  count(frost_in_most_Sensitive_period)
frost_in_most_Sensitive_period_count_year <- frost_in_most_Sensitive_period_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_in_most_Sensitive_period)
frost_in_most_Sensitive_period_count_year


### join the summary data together
frost_event_count_year
frost_in_Sensitive_period_count_year
frost_in_most_Sensitive_period_count_year
summary_frost_details <- rbind(frost_event_count_year, frost_in_Sensitive_period_count_year, frost_in_most_Sensitive_period_count_year)

summary_frost_details
days_GS

summary_frost_details <- left_join(summary_frost_details, days_GS)
summary_frost_details <- summary_frost_details %>% mutate(
  frost_event_percent = (frost_event_count/ days_GS)*100)

summary_frost_details

# Check_1_apri_2000 <- summary_frost_details %>%
#   filter(Sowing_date == "1-apr") %>%
#   filter(year == 2000)# 
# Check_1_apri_2000

unique(summary_frost_details$grouping)
unique(summary_frost_details$Sowing_date)

summary_frost_details$Sowing_date <- factor(summary_frost_details$Sowing_date , ordered = TRUE, 
                                            levels = c(
                                              "1-apr" ,
                                              "5-apr" ,
                                              "10-apr" ,
                                              "15-apr" ,
                                              "20-apr",
                                              "25-may",
                                              "1-may" ,
                                              "5-may" ,
                                              "10-may" ,
                                              "15-may" ,
                                              "20-may",
                                              "25-apr" 
                                            ))




plot1 <- summary_frost_details %>% filter(grouping == "frost" ) %>% 
  ggplot(aes(x = year, frost_event_percent, fill =grouping))+
  geom_bar(stat="identity")+
  facet_wrap(. ~ Sowing_date)+ 
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Fixed sowing dates as facet",
       x = "years",
       y = "Percentage of days classified as frost (-4 to 1)",
       caption = "Note; 1-19 frost events occured in sensitive period")
plot1

unique(summary_frost_details$Sowing_date)

plot2 <- summary_frost_details %>% 
  filter(grouping == "frost" ) %>% 
  filter(Sowing_date == "1-apr") %>% 
  ggplot(aes(x = year, frost_event_percent, fill =grouping))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Fixed sowing dates 1-Apr",
       x = "years",
       y = "Percentage of days classified as frost (-4 to 1)",
       caption = "Note; 1-19 frost events occured in sensitive period")
plot2


## Save files ----


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")
path_saved_files
ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_2022_2024", ".png" ),
       width = 20, height = 12, units = "cm")

ggsave(plot = plot2,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_10-May_2022_2024", ".png" ),
       width = 20, height = 12, units = "cm")

write_csv(summary_frost_details,
          file = paste0(path_saved_files,"/summary_frost_details_Lock_2022_2024", ".csv"))
