
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(zoo)

    
Lock_climate2015_2024 <- read_csv( "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/NeatClimate_18046_yrs2014_to_2024.csv")
str(Lock_climate2015_2024)

# Number of days for averaging  ------------------------------------------------
###ie the last 7 days or 5 days
days_for_ave <- 7

# Run as a loop for years -----------------------------------------------------

year_analysis <- c("2014", "2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024")


for(year_analysis in year_analysis){



## per year  --------------------
#year_analysis <- "2023"


Lock_climate_yr <- Lock_climate2015_2024 %>% filter(year == as.double(year_analysis))

# Remove data outside end of window ------------------------------------------- 

Lock_climate_yr <- Lock_climate_yr %>% 
  filter(date <= as.Date(paste0(year , "-05-30")))

# average rain and evap for specified days -------------------------------------
Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(sum_rain = rollsumr(rain, k =days_for_ave, fill= NA),
         sum_evap = rollsumr(evap, k =days_for_ave, fill= NA),
         rainfall_exceeds_evaporation = sum_rain -sum_evap)

# Did the conditions meet unkovich rule ----------------------------------------
Lock_climate_yr <- Lock_climate_yr %>% 
  mutate(Unkovich_conditions = case_when(
    rainfall_exceeds_evaporation >0 ~ "Sowing_break"
  ) )
         


# ## Quick check to see if it matches what I did in excel
# ## so far so good
# slice(Lock_climate_yr, -(1:20)) #igrone the first few clm
#Lock_climate_yr[10:30, c(10,6,7,16:19)] #just checking the clms I want to see. seems to match my excels example
#Lock_climate_yr_window[3:30, c(10,6,7,16:19)]

# Sowing April -------------------------------------------------------------

## dry sowing date 4 dates in April(4) and 4 in May(5)
dry_sowing_month <- "05"
dry_sowing_day_of_month <- "01" #5,10,15,20,25

## Sowing_window 01 -------------------------------------------------------------
dry_sowing_day_of_month <- "01"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
#keep the all "Sowing_break"
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
#keep first date
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)

## Sowing_window 05 -------------------------------------------------------------
dry_sowing_day_of_month <- "05"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)

## Sowing_window 10 -------------------------------------------------------------
dry_sowing_day_of_month <- "10"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 15 -------------------------------------------------------------
dry_sowing_day_of_month <- "15"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 20 -------------------------------------------------------------
dry_sowing_day_of_month <- "20"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 25 -------------------------------------------------------------
dry_sowing_day_of_month <- "25"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)
## Join the April sowing windows together --------------------------------------

April_sowing_windows <- rbind(Sowing_window_01, 
                              Sowing_window_05, 
                              Sowing_window_10, 
                              Sowing_window_15, 
                              Sowing_window_20,
                              Sowing_window_25)
rm(Sowing_window_01, 
   Sowing_window_05, 
   Sowing_window_10, 
   Sowing_window_15, 
   Sowing_window_20,
   Sowing_window_25)


# Sowing May -------------------------------------------------------------

## dry sowing date 4 dates in April(4) and 4 in May(5)
dry_sowing_month <- "05"
dry_sowing_day_of_month <- "01" #5,10,15,20,25

## Sowing_window 01 -------------------------------------------------------------
dry_sowing_day_of_month <- "01"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
#keep the all "Sowing_break"
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
#keep first date
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)

## Sowing_window 05 -------------------------------------------------------------
dry_sowing_day_of_month <- "05"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)

## Sowing_window 10 -------------------------------------------------------------
dry_sowing_day_of_month <- "10"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 15 -------------------------------------------------------------
dry_sowing_day_of_month <- "15"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 20 -------------------------------------------------------------
dry_sowing_day_of_month <- "20"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)


## Sowing_window 25 -------------------------------------------------------------
dry_sowing_day_of_month <- "25"
Lock_climate_yr_window <- Lock_climate_yr %>% 
  filter(date >= as.Date(paste0(year , "-",dry_sowing_month, "-", dry_sowing_day_of_month)))
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  filter(Unkovich_conditions == "Sowing_break")
Lock_climate_yr_window <- Lock_climate_yr_window %>% 
  group_by(Unkovich_conditions ) %>% 
  slice_min(date) %>% 
  mutate(sowing_window = paste0(dry_sowing_month, "-", dry_sowing_day_of_month))

name <- paste0("Sowing_window_", dry_sowing_day_of_month)
assign(name,Lock_climate_yr_window)
rm(Lock_climate_yr_window, dry_sowing_day_of_month)
## Join the May sowing windows together --------------------------------------

May_sowing_windows <- rbind(Sowing_window_01, 
                              Sowing_window_05, 
                              Sowing_window_10, 
                              Sowing_window_15, 
                              Sowing_window_20,
                              Sowing_window_25)
rm(Sowing_window_01, 
   Sowing_window_05, 
   Sowing_window_10, 
   Sowing_window_15, 
   Sowing_window_20,
   Sowing_window_25)

## Join the April and May sowing windows together ------------------------------
April_May_sowing_windows <- rbind(April_sowing_windows, May_sowing_windows)
name <- paste0("April_May_sowing_windows_", year_analysis)
assign(name,April_May_sowing_windows)
rm(April_sowing_windows, May_sowing_windows, April_May_sowing_windows)
}


ls(.GlobalEnv)

April_May_Sowing_window_2015_2024 <- rbind(
April_May_sowing_windows_2015,
April_May_sowing_windows_2016,
April_May_sowing_windows_2017,
April_May_sowing_windows_2018,
April_May_sowing_windows_2019 ,
April_May_sowing_windows_2020,
April_May_sowing_windows_2021 ,
April_May_sowing_windows_2022,
April_May_sowing_windows_2023,
April_May_sowing_windows_2024)

rm(April_May_sowing_windows_2015,
   April_May_sowing_windows_2016,
   April_May_sowing_windows_2017,
   April_May_sowing_windows_2018,
   April_May_sowing_windows_2019 ,
   April_May_sowing_windows_2020,
   April_May_sowing_windows_2021 ,
   April_May_sowing_windows_2022,
   April_May_sowing_windows_2023,
   April_May_sowing_windows_2024)


# What years sowing conditions were not met?
year_analysis_short <- c("2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024")
sowing_windows_list <- c(
  "04-01",
  "04-05",
  "04-10",
  "04-15" ,
  "04-20",
  "04-25",
  "05-01" ,
  "05-05" ,
  "05-10" ,
  "05-15"  ,
  "05-20"  ,
  "05-25"
)


df_sowing_windows_years <- expand.grid(year_analysis_short, sowing_windows_list)
names(df_sowing_windows_years)

df_sowing_windows_years <- df_sowing_windows_years %>% 
   rename(year = Var1, sowing_window = Var2)
# df_sowing_windows_years$year <- as.double(df_sowing_windows_years$year)
# str(df_sowing_windows_years)

df_sowing_windows_years$sowing_window <- as.character(df_sowing_windows_years$sowing_window)
str(df_sowing_windows_years)
df_sowing_windows_years$year <- as.character(df_sowing_windows_years$year)
df_sowing_windows_years$year <- as.numeric(df_sowing_windows_years$year) 

Small_April_May_Sowing_window_2015_2024 <- April_May_Sowing_window_2015_2024 %>% 
  select(year,sowing_window, Unkovich_conditions, date, sum_rain, sum_evap, rainfall_exceeds_evaporation )

str(Small_April_May_Sowing_window_2015_2024)
str(df_sowing_windows_years)
Complete_list_with_unk_rule <- left_join(df_sowing_windows_years, Small_April_May_Sowing_window_2015_2024)

names(Complete_list_with_unk_rule)
Complete_list_with_unk_rule <- Complete_list_with_unk_rule %>% 
  mutate(sowing_window_text = case_when(
    sowing_window == "04-01" ~ "01April",
    sowing_window == "04-05" ~ "05April",
    sowing_window == "04-10" ~ "10April",
    sowing_window == "04-15" ~ "15April",  
    sowing_window == "04-20" ~ "20April", 
    sowing_window == "04-25" ~ "25April", 
    
    sowing_window == "05-01" ~ "01May",
    sowing_window == "05-05" ~ "05May",
    sowing_window == "05-10" ~ "10May",
    sowing_window == "05-15" ~ "15May",  
    sowing_window == "05-20" ~ "20May", 
    sowing_window == "05-25" ~ "25May"
  ))

write_csv(Complete_list_with_unk_rule,
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/Results/Manual_check_yrs_unk_rule2015-2024_av7days.csv" )
