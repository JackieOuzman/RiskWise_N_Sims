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


### apply a generic GS 

## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 8 # Aug, it was 4 April


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 10 # Nov, it was 11 Oct

#File start date and end date
paste("Start date in file is: ",
      min(met_frost$date),
      ". End date in file is: ",
      max(met_frost$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(met_frost)

# Assign season type to climate data

met_frost <- met_frost %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

met_frost <- met_frost %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

met_frost <- met_frost %>% select(-start_end_GS_date)

met_frost <- met_frost %>%
  mutate(GS_definition = GS_defined_as)


met_frost_GS <- met_frost %>% filter(season == "gs")

## frost days
## Summaries data how many frost days in GS by year----
names(met_frost_GS)
str(met_frost_GS)

days_GS <- met_frost_GS %>% 
  group_by(year ) %>% 
  count(season)
days_GS <- days_GS %>%  rename(days_GS = n)
days_GS

frost_event_count_year <- met_frost_GS %>% 
  group_by(year, frost_event) %>% 
  count(frost_event)

frost_event_count_year <- frost_event_count_year %>% 
  rename(frost_event_count = n,
         grouping = frost_event) %>% 
  filter(grouping ==  "frost")
frost_event_count_year



summary_frost_details <- left_join(frost_event_count_year, days_GS)
summary_frost_details <- summary_frost_details %>% mutate(
  frost_event_percent = (frost_event_count/ days_GS)*100)
summary_frost_details_all_yrs <- left_join(days_GS, summary_frost_details)

summary_frost_details_all_yrs <- summary_frost_details_all_yrs %>% 
  select(year, days_GS, frost_event_count, frost_event_percent) %>% 
  mutate(GS_definition = GS_defined_as) %>% 
  mutate(frost_event_count = ifelse(is.na(frost_event_count), 0, frost_event_count),
         frost_event_percent = ifelse(is.na(frost_event_percent), 0, frost_event_percent))

unique(days_GS$days_GS)
GS_defined_as


plot1 <- summary_frost_details_all_yrs %>% 
  filter(year >= 1957) %>% 
  ggplot(aes(x = as.factor(year), frost_event_count ))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 11))+
  labs(title = "Frost Risk at Lock climate station number 18046",
       subtitle = paste0("Standard GS of ", unique(days_GS$days_GS)," days, defined as ", GS_defined_as),
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
       subtitle = paste0("Standard GS of ", unique(days_GS$days_GS)," days, defined as ", GS_defined_as),
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
       subtitle = paste0("Standard GS of ", unique(days_GS$days_GS)," days, defined as ", GS_defined_as),
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
       subtitle = "Standard GS of 214 day. 1/4 to 1/11",
       x = "years",
       y = "Percentage of days classified as frost (< 1)"
  )
plot2_2015_2024perc

                                                        


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing","version5" , "Results")
path_saved_files
ggsave(plot = plot1,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std_GS", ".png" ),
       width = 20, height = 12, units = "cm")


ggsave(plot = plot1_2015_2024,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std_GS_2014_2024", ".png" ),
       width = 20, height = 12, units = "cm")



ggsave(plot = plot2_perc,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std_GS_Prec", ".png" ),
       width = 20, height = 12, units = "cm")


ggsave(plot = plot2_2015_2024perc,
       filename = paste0(path_saved_files,"/Climate_Frost_days_Lock_std_GS_2014_2024_Prec", ".png" ),
       width = 20, height = 12, units = "cm")



write_csv(summary_frost_details_all_yrs,
          file = paste0(path_saved_files,"/summary_frost_details_Lock_Std_GS", ".csv"))

