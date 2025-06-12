## Run step 1 to get climate data in the correct format

## This is an extra step to deal with permeations created when factorising a sowing rule with a window


library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

# Download Daily climate files created in step 1 (with frost days) -------------------------------------------------------
#"X:\Riskwi$e\Dry_sowing\Lock\Results"
met_frost <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Results/Fost_details_18046.csv")
str(met_frost)

# Download Daily APSIM output -------------------------------------------------------
Lock_Daily_APSIM <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/APSIM_runs/Lock_Water_balance_v5 WB_10mm.Daily.csv", 
                                  col_types = cols(Clock.Today = col_date(format = "%Y-%m-%d")))


names(Lock_Daily_APSIM)
str(Lock_Daily_APSIM)
max(Lock_Daily_APSIM$Clock.Today)
min(Lock_Daily_APSIM$Clock.Today)

## subset APSIM data

Lock_Daily_APSIM <- Lock_Daily_APSIM %>% select(
  CheckpointName,
  SimulationName,
  StartDate,
  EndDate,
  Clock.Today ,
  Wheat.Phenology.Zadok.Stage,
  Wheat.Phenology.CurrentStageName,
  Wheat.Phenology.Stage,
  Yield ,
  Yield_Adj_t_ha ,
  
)


## Make a 2 new clm called 'Sowing_date' that will match the other files and a temp file to work out what to keep



Lock_Daily_APSIM <- Lock_Daily_APSIM %>% 
  mutate(Sowing_window = paste0(StartDate, " to ",EndDate ))
unique(Lock_Daily_APSIM$Sowing_window)
# what I want to keep is:
sowing_window_retain <- c("1-apr to 30-jun",
                          "5-apr to 30-jun",
                          "10-apr to 30-jun",
                          "15-apr to 30-jun",
                          "20-apr to 30-jun",
                          "25-apr to 30-jun",
                          "30-apr to 30-jun",
                          
                          "1-may to 30-jun",
                          "5-may to 30-jun",
                          "10-may to 30-jun",
                          "15-may to 30-jun",
                          "20-may to 30-jun",
                          "25-may to 30-jun",
                          "30-may to 30-jun",
                          
                          "1-jun to 30-jun",
                          "5-jun to 30-jun",
                          "10-jun to 30-jun",
                          "15-jun to 30-jun",
                          "20-jun to 30-jun",
                          "25-jun to 30-jun"
)

## keep only the sowing dates I want (I think this is all of them)

sowing_window_retain <- Lock_Daily_APSIM %>% 
  filter(Sowing_window %in% sowing_window_retain)
unique(sowing_window_retain$Sowing_window)

Lock_Daily_APSIM <- Lock_Daily_APSIM %>% 
  rename(Sowing_date = StartDate)
names(Lock_Daily_APSIM)




### Make a new clm with Sensitive period of frost = 6.49 - 9.5  ------------------
summary(Lock_Daily_APSIM)
str(Lock_Daily_APSIM)

Lock_Daily_APSIM <- Lock_Daily_APSIM %>% 
  mutate(frost_Sensitive_period = 
           case_when(
             between(Wheat.Phenology.Stage, 6.49, 9.5) ~ "frost_sensitive_period",
             .default = "outside_Sensitive_period"
           ))
### Make a new Most sensitive period 8-9.5  -----------------------------------------------
Lock_Daily_APSIM <- Lock_Daily_APSIM %>% 
  mutate(frost_most_Sensitive_period = 
           case_when(
             between(Wheat.Phenology.Stage, 8.0, 9.5) ~ "frost_most_sensitive_period",
             .default = "outside_period"
           ))

#this check is for above code
#check <- Lock_Daily_APSIM %>%  filter(between(Wheat.Phenology.Stage, 6.49, 9.5))
#rm(check)

## Append met_frost ------------------------------------------------------------
str(Lock_Daily_APSIM)
str(met_frost)

Lock_Daily_APSIM$Clock.Today <- as.Date(Lock_Daily_APSIM$Clock.Today)
Lock_APSIM_Met <- left_join(Lock_Daily_APSIM,met_frost, by = join_by("Clock.Today" == "date"))







# Create new clms to classify if frost occurred in frost sensitive period or most sensitive period etc ----


## Create a new clm which defines the season and only keep data in GS ----
unique(Lock_APSIM_Met$Sowing_date)

## use the phenology stages to work out the GS - add a clm with two dates marked as start and end
Lock_APSIM_Met <- Lock_APSIM_Met %>% mutate(
  start_end_GS = case_when(
    Wheat.Phenology.CurrentStageName == "Sowing"  ~ "start_gs",
    Wheat.Phenology.CurrentStageName == "HarvestRipe"  ~ "end_gs",
    TRUE ~ NA))
### fill the blanks
Lock_APSIM_Met <- Lock_APSIM_Met %>% fill(start_end_GS) %>%
  mutate(
    season = case_when(
      start_end_GS == "start_gs" ~ "gs",
      TRUE ~ "other"))
## not quite right here I still need to keep   "HarvestRipe"
Lock_APSIM_Met <- Lock_APSIM_Met %>% mutate(
  season = case_when(
    Wheat.Phenology.CurrentStageName == "HarvestRipe"  ~ "gs",
    TRUE ~ season))


Lock_APSIM_Met <- Lock_APSIM_Met %>% select(-start_end_GS)  
str(Lock_APSIM_Met)





## Only keep data in the season
Lock_APSIM_Met_gs <- Lock_APSIM_Met %>% filter(season == "gs")

## new clm when frost_Sensitive_period and frost----
unique(Lock_APSIM_Met_gs$frost_event)
unique(Lock_APSIM_Met_gs$frost_Sensitive_period)


## new clm when a frost event occur is sensitive period
Lock_APSIM_Met_gs <- Lock_APSIM_Met_gs %>% 
  mutate(
    frost_in_Sensitive_period = case_when(
      frost_event == "frost" & frost_Sensitive_period == "frost_sensitive_period"~ "frost_in_Sensitive_period",
      frost_event == "frost" & frost_Sensitive_period == "outside_Sensitive_period"~ "no_frost_Sensitive_period",
      
      frost_event == "non_frost" & frost_Sensitive_period == "frost_sensitive_period"~ "no_frost_Sensitive_period",
      frost_event == "non_frost" & frost_Sensitive_period == "outside_Sensitive_period"~ "no_frost_Sensitive_period",
      
      TRUE ~ "no_frost_Sensitive_period"))

unique(Lock_APSIM_Met_gs$frost_most_Sensitive_period)   
Lock_APSIM_Met_gs <- Lock_APSIM_Met_gs %>% 
  mutate(
    frost_in_most_Sensitive_period = case_when(
      
      frost_event == "frost" & frost_most_Sensitive_period == "frost_most_sensitive_period"~ "frost_in_most_Sensitive_period",
      frost_event == "frost" & frost_most_Sensitive_period == "outside_period"~ "no_frost_most_Sensitive_period",
      
      frost_event == "non_frost" & frost_most_Sensitive_period == "frost_most_sensitive_period"~ "no_frost_most_Sensitive_period",
      frost_event == "non_frost" & frost_most_Sensitive_period == "outside_period"~ "no_frost_most_Sensitive_period",
      
      TRUE ~ "no_frost_most_Sensitive_period"))

#test
str(Lock_APSIM_Met_gs)
test <- Lock_APSIM_Met_gs %>% filter(year == 2024) %>%  filter(Sowing_date== "10-may") %>% 
  select(Clock.Today,Wheat.Phenology.Stage, mint, frost_event, 
         frost_Sensitive_period, frost_in_Sensitive_period,
         frost_most_Sensitive_period, frost_in_most_Sensitive_period)

rm(test)


      

# Save file
write.csv(Lock_APSIM_Met_gs ,
          "X:/Riskwi$e/Dry_sowing/Lock/Results/WB_10mm/WB_10mmDry_sowing_Lock_factor_with_met_18046_v2_gs.csv", row.names = FALSE )


## Summaries the frost days




## Summaries data how many frost days in GS by year----
names(Lock_APSIM_Met_gs)
str(Lock_APSIM_Met_gs)

days_GS <- Lock_APSIM_Met_gs %>% 
  group_by(year,Sowing_date ) %>% 
  count(season)
days_GS <- days_GS %>%  rename(days_GS = n)
days_GS

frost_event_count_year <- Lock_APSIM_Met_gs %>% 
  group_by(year,Sowing_date) %>% 
  count(frost_event)

frost_event_count_year <- frost_event_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_event)
frost_event_count_year


frost_in_Sensitive_period_count_year <- Lock_APSIM_Met_gs %>% 
  group_by(year,Sowing_date) %>% 
  count(frost_in_Sensitive_period)
frost_in_Sensitive_period_count_year <- frost_in_Sensitive_period_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_in_Sensitive_period)
frost_in_Sensitive_period_count_year

frost_in_most_Sensitive_period_count_year <- Lock_APSIM_Met_gs %>% 
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


#remove the temp files 
rm( days_GS, 
    frost_event_count_year,
    frost_in_Sensitive_period_count_year,
    frost_in_most_Sensitive_period_count_year
)



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
                                              "25-apr" ,
                                              "30-apr" ,
                                              
                                              "1-may" ,
                                              "5-may" ,
                                              "10-may" ,
                                              "15-may" ,
                                              "20-may",
                                              "25-may",
                                              "30-may",
                                              
                                              "1-jun" ,
                                              "5-jun" ,
                                              "10-jun" ,
                                              "15-jun" ,
                                              "20-jun",
                                              "25-jun"
                                            ))




plot1 <- summary_frost_details %>% filter(grouping == "frost" ) %>% 
  ggplot(aes(x = year, frost_event_percent, fill =grouping))+
  geom_bar(stat="identity")+
  facet_wrap(. ~ Sowing_date)+ 
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only.",
       x = "years",
       y = "Percentage of days classified as frost (-4 to 1)",
       caption = "Note; 1-19 frost events occured in sensitive period")
plot1

plot2 <- summary_frost_details %>% 
  filter(grouping == "frost" ) %>% 
  filter(Sowing_date == "10-may") %>% 
  ggplot(aes(x = year, frost_event_percent, fill =grouping))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Fixed sowing dates 10-May",
       x = "years",
       y = "Percentage of days classified as frost (-4 to 1)",
       caption = "")
plot2


## Save files ----


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Results", "WB_10mm")
path_saved_files
ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_v2", ".png" ),
       width = 20, height = 12, units = "cm")

ggsave(plot = plot2,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_10-May_v2", ".png" ),
       width = 20, height = 12, units = "cm")

write_csv(summary_frost_details,
          file = paste0(path_saved_files,"/WB_10mm_summary_frost_details_Lock_v2", ".csv"))
