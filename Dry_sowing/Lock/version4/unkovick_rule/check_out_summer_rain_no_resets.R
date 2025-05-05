# This file is for importing met file assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

Est_sowing_conditions_v1 <- read_excel("X:/Riskwi$e/Dry_sowing/Lock/Est_soil_starting_conditions/Est_sowing_conditions_v1.xlsx", 
                                       col_types = c("text", "numeric", "text", 
                                                     "numeric", "text", "date", "text", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))




### sum of rainfall from '10Nov' to sowing date

names(Est_sowing_conditions_v1)
str(Est_sowing_conditions_v1)

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>%
  mutate(
    date = Clock.Today ,
    year = year(date),
    month = month(date),
    day = day(date),
    day_of_month = day(date),
    month_name = lubridate::month(date, label = TRUE),
    site = paste0("Lock", "_", 018046)
  )

### get the ESW at sowing also the Soil.Water.MM(1) through to (7)

ESW_at_sowing <- Est_sowing_conditions_v1 %>%
  filter(Wheat.Phenology.CurrentStageName == "Sowing") %>%
  select(
    `year`,
    `Wheat.Phenology.CurrentStageName`,
    `date`,
    `Sum_InitialValuesMM`,
    `SumESW`,
    `Sum_MM`,
    `Sum(Soil.SoilWater.PAWmm`,
    `Soil.Water.MM(1)`,
    `Soil.Water.MM(2)`,
    `Soil.Water.MM(3)` ,
    `Soil.Water.MM(4)`,
    `Soil.Water.MM(5)`,
    `Soil.Water.MM(6)`,
    `Soil.Water.MM(7)`
  )


## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 11

#File start date and end date
paste("Start date in file is: ",
      min(Est_sowing_conditions_v1$date),
      ". End date in file is: ",
      max(Est_sowing_conditions_v1$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(Est_sowing_conditions_v1)

# Assign season type to climate data

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

Est_sowing_conditions_v1 <- Est_sowing_conditions_v1 %>% select(-start_end_GS_date)
str(Est_sowing_conditions_v1)

### what are the sowing dates for the 15 years
sow_dates_by_year <- Est_sowing_conditions_v1 %>% 
filter (Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  select(year, date,  Wheat.Phenology.CurrentStageName,  SumESW,Sum_MM )

list_sow_dates <- sow_dates_by_year[,2]


summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1960-11-01'), 
                 as.Date('1961-04-01'))) #small to big dates
summer_rain1961 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)  

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1961-11-01'), 
                 as.Date('1962-04-01'))) #small to big dates
summer_rain1962 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1962-11-01'), 
                 as.Date('1963-04-01'))) #small to big dates
summer_rain1963 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1963-11-01'), 
                 as.Date('1964-04-01'))) #small to big dates
summer_rain1964 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1964-11-01'), 
                 as.Date('1965-04-01'))) #small to big dates
summer_rain1965 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1965-11-01'), 
                 as.Date('1966-04-01'))) #small to big dates
summer_rain1966 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1966-11-01'), 
                 as.Date('1967-04-01'))) #small to big dates
summer_rain1967 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1967-11-01'), 
                 as.Date('1968-04-01'))) #small to big dates
summer_rain1968 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1968-11-01'), 
                 as.Date('1969-04-01'))) #small to big dates
summer_rain1969 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1969-11-01'), 
                 as.Date('1970-04-01'))) #small to big dates
summer_rain1970 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1970-11-01'), 
                 as.Date('1971-04-01'))) #small to big dates
summer_rain1971<- sum(summer_rain$Weather.Rain, na.rm = TRUE)

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1971-11-01'), 
                 as.Date('1972-04-01'))) #small to big dates
summer_rain1972<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1972-11-01'), 
                 as.Date('1973-04-01'))) #small to big dates
summer_rain1973<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1973-11-01'), 
                 as.Date('1974-04-01'))) #small to big dates
summer_rain1974<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 

summer_rain <- Est_sowing_conditions_v1 %>% 
  filter(between(date, 
                 as.Date('1974-11-01'), 
                 as.Date('1975-04-01'))) #small to big dates
summer_rain1975<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 


#year = seq(as.Date("1961/1/1"), as.Date("1975/1/1"), by = "year")
year <- seq(1961,1975)
summer_rain <- c(summer_rain1961,summer_rain1962, summer_rain1963, summer_rain1964, summer_rain1965,
                 summer_rain1966, summer_rain1967, summer_rain1968, summer_rain1969, summer_rain1970,
                 summer_rain1971, summer_rain1972, summer_rain1973, summer_rain1974, summer_rain1975)
                 


Sum_summer_rain_1961_75 <- data.frame(year = year,
                                         summer_rain = summer_rain)
rm(summer_rain1961,summer_rain1962, summer_rain1963, summer_rain1964, summer_rain1965,
   summer_rain1966, summer_rain1967, summer_rain1968, summer_rain1969, summer_rain1970,
   summer_rain1971, summer_rain1972, summer_rain1973, summer_rain1974, summer_rain1975)
rm(year, summer_rain)


## summer rain is 1961 so I need to trim the ESW
ESW_at_sowing <- ESW_at_sowing %>%  filter(year != "1960")

soil_water_summer_rain <- left_join(Sum_summer_rain_1961_75, ESW_at_sowing)
soil_water_summer_rain

#######################################################################

inital_Soil_water_values <- soil_water_summer_rain %>% distinct( Sum_InitialValuesMM)

plot_summer_rain <- soil_water_summer_rain %>% 
  ggplot(aes(x = year, summer_rain))+
  
  geom_line()+
  annotate("text", x =1963, y = 60, label = "Sum summer rainfall", colour = "black")+
  
  geom_line(aes(y= SumESW), colour= "blue")+
  annotate("text", x =1963, y = 115, label = "Sum APSIM ESW at sowing", colour = "blue")+
  
  geom_line(aes(y= Sum_MM), colour= "darkolivegreen3")+
  annotate("text", x =1963, y = 200, label = "Sum APSIM MM at sowing", colour = "darkolivegreen3")+
  
  # geom_line(aes(y= `Sum(Soil.SoilWater.PAWmm`), colour= "cyan3")+
  # annotate("text", x =1963, y = 150, label = "Sum APSIM PAW at sowing", colour = "cyan3")+
  
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Summer rainfall and APSIM soil water ",
       subtitle = "Station Lock. Number: 18046. Years 1961-195",
       x = "",
       y = "",
       caption = paste0("Inital APSIM soil water value MM", inital_Soil_water_values, ". Also note: Sum PAW and ESW are same values"))
plot_summer_rain
