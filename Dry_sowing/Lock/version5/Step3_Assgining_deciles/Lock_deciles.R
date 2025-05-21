# This file is for assigning rainfall decile years
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Download Daily climate files -------------------------------------------------------

climate <- read.table("X:/Riskwi$e/met_files/LOCK_18046.sim", 
                      skip = 22, header = TRUE, sep ="")
climate <- climate [-1,]
### need to make a clm that has all the dates not just day of year and year
str(climate)
climate$rain <- as.character(climate$rain)
climate$rain <- as.double(climate$rain)

#create a date clm # note you need to be aware of the last day of downloaded met data

## Assign dates, site detial and format -------------------------------------------------
download_date <- read_csv("X:/Riskwi$e/met_files/LOCK_18046.sim",
                          col_names = FALSE, skip = 7)
download_date <-download_date[1,1] #just the row with the download date
download_date <-stringr::str_extract(download_date, "[[:digit:]]+") #just the numbers
download_date <- as.Date(as.character(download_date),format="%Y%m%d")

#minus one day from download
download_date <- lubridate::ymd(download_date) - days(1)
str(download_date)

download_date <-"20250403" # "2025/04/03"
download_date <- as.Date(as.character(download_date),format="%Y%m%d")
download_date



climate <- climate %>% 
  mutate(date = seq(as.Date("1900/1/1"), download_date, "days"))
# set date as a date
climate <- climate %>% 
  mutate(year = year(date),
         month =month(date),
         day_of_month = day(date),
         month_name = lubridate::month(date, label = TRUE),
         site = paste0("Lock","_", 018046))
str(climate)

climate$maxt <- as.numeric(climate$maxt)
climate$mint <- as.numeric(climate$mint)

## Write out neat climate file ------------------------------------------------
write.csv(climate ,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Deciles/NeatClimate_18046.csv", row.names = FALSE )
          
          
## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 11

#File start date and end date
paste("Start date in file is: ",
      min(climate$date),
      ". End date in file is: ",
      max(climate$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(climate)

# Assign season type to climate data

climate <- climate %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

climate <- climate %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

climate <- climate %>% select(-start_end_GS_date)

## Dates to use for analysis and filter climate data ---------------------------
min(climate$date)
max(climate$date)

start <- "1957-01-01"
end <- "2024-12-31"

# Filter dataset based on years 
climate <- climate %>% filter(between(date, as.Date(start), as.Date(end)))


## Plot rainfall data grouped into season type ---------------------------------

plot_rainfall_season_type <- climate %>%
  group_by(year, season) %>%
  summarise(sum_rain_season = sum(rain, na.rm = TRUE)) %>%
  
  
  ggplot(aes(x = year, y = sum_rain_season, fill = season)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sum of rain per year",
    subtitle = paste0(
    "Station name: ",
    distinct(climate, site)
    ),
    caption = paste0("Years included : ",
                     year(min(climate$date)) ,
                     " to ",
                     year(max(climate$date)),
                     ". GS defined as: ", 
                     GS_defined_as),
    
    y = "sum of rain")
plot_rainfall_season_type


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "version5", "Deciles")

ggsave(plot = plot_rainfall_season_type,
       filename = paste0(path_saved_files,"/plot_rainfall_season_type_Lock2023", ".png" ),
       width = 20, height = 12, units = "cm")


## Group by year and season type and report sum of rain -------------------------
rain_season <- climate %>%
  group_by(year, season) %>%
  summarise(sum_rain_season = sum(rain, na.rm = TRUE)) 
str(rain_season)
site <- "Lock_18046"

rain_season_wide <- rain_season %>% 
  pivot_wider(
    names_from = season,
    values_from =sum_rain_season 
  )
str(rain_season_wide)
rain_season_wide <- ungroup(rain_season_wide)

rain_season_wide <- rain_season_wide %>% 
  mutate(site = site,
         years_included = paste0(year(min(climate$date)) ," to ", year(max(climate$date))),
         GS = GS_defined_as) 
str(rain_season_wide)        


## new variable gs rain + 0.30 of summer --------------------------------------

rain_season_wide <- rain_season_wide %>% 
  mutate(gs_plus30_summer = gs+ (0.30*summer))

## Cal percentiles for each year -----------------------------------------------

GS_30_summer_decile_max_rain <- quantile(rain_season_wide$gs_plus30_summer, 
                               c(.1, .2, .3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
GS_30_summer_deciles_names <- c("decile_1", "decile_2", "decile_3", "decile_4", "decile_5", "decile_6", "decile_7", "decile_8", "decile_9", "decile_10")

GS_30_summer_decile_table <- data.frame(GS_30_summer_decile_max_rain, GS_30_summer_deciles_names, row.names = NULL)
GS_30_summer_decile_table <- mutate(GS_30_summer_decile_table, GS_30_summer_decile_min_rain = lag(GS_30_summer_decile_max_rain+0.1))


#add in the min value to table
GS_30_summer_decile_table[1,3] <-  min(rain_season_wide$gs_plus30_summer)
GS_30_summer_decile_table <- GS_30_summer_decile_table %>%  
  select (GS_30_summer_deciles_names, GS_30_summer_decile_min_rain, GS_30_summer_decile_max_rain)

GS_30_summer_decile_table

GS_30_summer_decile_table <- GS_30_summer_decile_table %>% 
  mutate( site = site,
                 years_included = paste0(year(min(climate$date)) ," to ", year(max(climate$date))),
                 GS = GS_defined_as) 



GS_30_summer_decile_table_df <- as.data.frame(GS_30_summer_decile_table)
GS_30_summer_decile_table_df <- GS_30_summer_decile_table_df %>% 
  mutate(across(where(is.numeric), round, 2))
GS_30_summer_decile_table_df


rain_season_wide <- rain_season_wide %>%
  mutate(
    decile = case_when(
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[1,2],1) ,round(GS_30_summer_decile_table_df[1,3],1) ) ~ "decile_1",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[2,2],1) ,round(GS_30_summer_decile_table_df[2,3],1) ) ~ "decile_2",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[3,2],1) ,round(GS_30_summer_decile_table_df[3,3],1) ) ~ "decile_3",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[4,2],1) ,round(GS_30_summer_decile_table_df[4,3],1) ) ~ "decile_4",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[5,2],1) ,round(GS_30_summer_decile_table_df[5,3],1) ) ~ "decile_5",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[6,2],1) ,round(GS_30_summer_decile_table_df[6,3],1) ) ~ "decile_6",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[7,2],1) ,round(GS_30_summer_decile_table_df[7,3],1) ) ~ "decile_7",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[8,2],1) ,round(GS_30_summer_decile_table_df[8,3],1) ) ~ "decile_8",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[9,2],1) ,round(GS_30_summer_decile_table_df[9,3],1) ) ~ "decile_9",
      between(round(gs_plus30_summer, 1), round(GS_30_summer_decile_table_df[10,2],1) ,round(GS_30_summer_decile_table_df[10,3],1) ) ~ "decile_10",
      
      TRUE                      ~ "other"
    )
  )

path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Lock", "Dry_sowing", "version5","Deciles")


write_csv(GS_30_summer_decile_table, 
          file =paste0(path_saved_files, "/GS_30_summer_decile_table_Lock18046", ".csv"))

write_csv(rain_season_wide, 
          file =paste0(path_saved_files, "/deciles per year_Lock18046", ".csv"))
