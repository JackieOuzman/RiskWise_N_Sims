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




# Download Daily next gen files  -----------------------------------------------


Dry_sowing_Lock_factor <- read_excel("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version4_unkovich_rule/unkovich_factor_phen_dates_v4.xlsx", 
                                     col_types = c("text", "numeric", "text", 
                                                   "numeric", "text", "text", "text", 
                                                   "text", "date", "numeric", "text", 
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
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric"))
str(Dry_sowing_Lock_factor)
names(Dry_sowing_Lock_factor)


Dry_sowing_Lock_factor <- Dry_sowing_Lock_factor %>% select(
  StartDate,
  #Sowing_date,  #(Sowing_date)
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
  mutate(PAW_Sum = sum(`Soil.Water.PAW(1)`,                         
         `Soil.Water.PAW(2)`,
         `Soil.Water.PAW(3)`,                         
         `Soil.Water.PAW(4)`,
         `Soil.Water.PAW(5)`,                         
         `Soil.Water.PAW(6)`,
         `Soil.Water.PAW(7)`,  na.rm = TRUE)) %>% 
           rename(Sowing_date = StartDate) #new name = old name

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
# I am having a lot of trouble with this code sometime it work? WHY? This hack is a bit sloppy but the results are better


Dry_sowing_Lock_factor_with_met <-Dry_sowing_Lock_factor_with_met %>% 
  mutate(frost_in_Sensitive_period = paste0(frost_event, "_", frost_Sensitive_period)) %>% 
  mutate(
    frost_in_Sensitive_period = case_when(
      frost_in_Sensitive_period == "frost_frost_sensitive_period" ~ "frost_in_sensitive_period",
      TRUE ~ "no_frost_Sensitive_period")
    )

unique(Dry_sowing_Lock_factor_with_met$frost_in_Sensitive_period)


Dry_sowing_Lock_factor_with_met <-Dry_sowing_Lock_factor_with_met %>% 
  mutate(frost_in_most_Sensitive_period = paste0(frost_event, "_", frost_most_Sensitive_period))

unique(Dry_sowing_Lock_factor_with_met$frost_in_most_Sensitive_period)
Dry_sowing_Lock_factor_with_met <-Dry_sowing_Lock_factor_with_met %>% 
  mutate(frost_in_most_Sensitive_period = paste0(frost_event, "_", frost_most_Sensitive_period)) %>% 
  mutate(
    frost_in_most_Sensitive_period = case_when(
      frost_in_most_Sensitive_period == "frost_frost_most_sensitive_period" ~ "frost_in_most_sensitive_period",
      TRUE ~ "no_frost_most_Sensitive_period")
  )

#test
str(Dry_sowing_Lock_factor_with_met)
test <- Dry_sowing_Lock_factor_with_met %>% 
  filter(year == 2023) %>%  
  filter(Sowing_date == "10-may") %>% 
  filter(frost_event == "frost") %>% 
  select(frost_event,
         frost_Sensitive_period ,
         frost_most_Sensitive_period,
         frost_in_Sensitive_period,
         frost_in_most_Sensitive_period )
test

# Save file
write.csv(Dry_sowing_Lock_factor_with_met ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/Results/Dry_sowing_Lock_factor_with_met_18046.csv", row.names = FALSE )


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


summary_frost_details

plot1 <- summary_frost_details %>% filter(grouping == "frost" ) %>% 
  #ggplot(aes(x = as.factor(year), frost_event_percent, fill =grouping))+
  ggplot(aes(x = as.factor(year), frost_event_count , fill =grouping))+
  geom_bar(stat="identity")+
  facet_wrap(. ~ Sowing_date)+ 
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  #scale_x_discrete(breaks=c(1960, 1970, 1980, 1990, 2000, 2010, 2020))+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Sowing dates as facet",
       x = "years",
       y = "Count of days classified as frost (-4 to 1)",
       #caption = "Note; 1-19 frost events occured in sensitive period"
       )
plot1

plot2 <- summary_frost_details %>% 
  filter(grouping == "frost" ) %>% 
  filter(Sowing_date == "10-may") %>% 
  ggplot(aes(x = as.factor(year), frost_event_percent, fill =grouping))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Climate station Lock 18046",
       subtitle = "Days from sowing to harvest only. Sowing dates 10-May",
       x = "years",
       y = "Percentage of days classified as frost (-4 to 1)",
       caption = "")
plot2


## Save files ----


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "Results")
path_saved_files
ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock", ".png" ),
       width = 20, height = 12, units = "cm")

ggsave(plot = plot2,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_10-May", ".png" ),
       width = 20, height = 12, units = "cm")

write_csv(summary_frost_details,
          file = paste0(path_saved_files,"/summary_frost_details_Lock", ".csv"))

