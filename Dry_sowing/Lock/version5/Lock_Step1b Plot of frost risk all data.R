# This file is for assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)





# Download Daily climate files created in step 1 (with frost days) -------------------------------------------------------
                     
met_frost <- read_csv("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Fost_details_18046.csv")
str(met_frost)


met_frost <- met_frost %>% 
  mutate(year = year(date),
         month =month(date),
         day = day(date),
         day_of_month = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("Lock","_", 018046))
str(met_frost)


### apply a Frost risk period with the  

## Define the frost risk (FR) period and assign season type ---------------------------------

Day_start_FR_rainfall <- 1
Month_start_FR_rainfall <- 8 # Aug, it was 4 April


Day_end_FR_rainfall <- 1
Month_end_FR_rainfall <- 10 # Nov, it was 11 Oct

#File start date and end date
paste("Start date in file is: ",
      min(met_frost$date),
      ". End date in file is: ",
      max(met_frost$date))

FR_defined_as <- paste0(
  Day_start_FR_rainfall, "/", Month_start_FR_rainfall,
  " to ", Day_end_FR_rainfall, "/", Month_end_FR_rainfall)

str(met_frost)

# Assign season type to climate data

met_frost <- met_frost %>% mutate(
  start_end_FR_date = case_when(
    month == Month_start_FR_rainfall & day_of_month == Day_start_FR_rainfall ~ "start_FR",
    month == Month_end_FR_rainfall & day_of_month == Day_end_FR_rainfall ~ "end_FR",
    TRUE ~ NA))

# Fill the blanks

met_frost <- met_frost %>% fill(start_end_FR_date) %>%
  mutate(
    season = case_when(
      start_end_FR_date == "start_FR" ~ "FR",
      TRUE ~ "outside_FR"))

met_frost <- met_frost %>% select(-start_end_FR_date)

met_frost <- met_frost %>%
  mutate(FR_definition = FR_defined_as)


met_frost_FR <- met_frost %>% filter(season == "FR")

## frost days
## Summaries data how many frost days in FR by year----
names(met_frost_FR)
str(met_frost_FR)

days_FR <- met_frost_FR %>% 
  group_by(year ) %>% 
  count(season)
days_FR <- days_FR %>%  rename(days_FR = n)
days_FR

frost_event_count_year <- met_frost_FR %>% 
  group_by(year, frost_event) %>% 
  count(frost_event)

frost_event_count_year <- frost_event_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_event) %>% 
  filter(grouping ==  "frost")
frost_event_count_year



summary_frost_details <- left_join(frost_event_count_year, days_FR)
summary_frost_details <- summary_frost_details %>% mutate(
  frost_event_percent = (frost_event_count/ days_FR)*100)
summary_frost_details_all_yrs <- left_join(days_FR, summary_frost_details)

summary_frost_details_all_yrs <- summary_frost_details_all_yrs %>% 
  select(year, days_FR, frost_event_count, frost_event_percent) %>% 
  mutate(FR_definition = FR_defined_as) %>% 
  mutate(frost_event_count = ifelse(is.na(frost_event_count), 0, frost_event_count),
         frost_event_percent = ifelse(is.na(frost_event_percent), 0, frost_event_percent))

unique(days_FR$days_FR)
FR_defined_as


plot1 <- summary_frost_details_all_yrs %>% 
  filter(year >= 1957) %>% 
  ggplot(aes(x = as.factor(year), frost_event_count ))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 11))+
  labs(title = "Frost Risk at Lock climate station number 18046",
       subtitle = paste0("Period of frost risk ", unique(days_FR$days_FR)," days, defined as ", FR_defined_as),
       x = "years",
       y = "Count of days classified as frost (< 1)"
  )
plot1

plot1_2015_2024 <- summary_frost_details_all_yrs %>% 
  filter(year>= 2014) %>% 
  ggplot(aes(x = as.factor(year), frost_event_count ))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 11))+
  labs(title = "Frost Risk at Lock climate station number 18046",
       subtitle = paste0("Period of frost risk ", unique(days_FR$days_FR)," days, defined as ", FR_defined_as),
       x = "years",
       y = "Count of days classified as frost (< 1)"
  )
plot1_2015_2024


plot2_perc <- summary_frost_details_all_yrs %>% 
  filter(year >= 1957) %>% 
  ggplot(aes(x = as.factor(year), frost_event_percent ))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Frost Risk at Lock climate station number 18046",
       subtitle = paste0("Period of frost risk ", unique(days_FR$days_FR)," days, defined as ", FR_defined_as),
       x = "years",
       y = "Percentage of days classified as frost (< 1)"
  )
plot2_perc

plot2_2015_2024perc <- summary_frost_details_all_yrs %>% 
  filter(year>= 2014) %>% 
  ggplot(aes(x = as.factor(year), frost_event_percent ))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Frost Risk at Lock climate station number 18046",
       subtitle = "Standard FR of 214 day. 1/4 to 1/11",
       x = "years",
       y = "Percentage of days classified as frost (< 1)"
  )
plot2_2015_2024perc

                                                        


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std_", ".png" ),
       width = 20, height = 12, units = "cm")


ggsave(plot = plot1_2015_2024,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std__2014_2024", ".png" ),
       width = 20, height = 12, units = "cm")



ggsave(plot = plot2_perc,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std__Prec", ".png" ),
       width = 20, height = 12, units = "cm")


ggsave(plot = plot2_2015_2024perc,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std__2014_2024_Prec", ".png" ),
       width = 20, height = 12, units = "cm")



write_csv(summary_frost_details_all_yrs,
          file = paste0(path_saved_files,"/summary_frost_details_Lock_Std_", ".csv"))

