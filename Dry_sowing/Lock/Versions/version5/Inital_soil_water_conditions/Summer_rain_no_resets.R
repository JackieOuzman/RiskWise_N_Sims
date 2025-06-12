# This file is for importing met file assigning frost days
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

Est_sowing_conditions_v1_2014_24 <- read_excel("X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/est_starting_soil_conditions/Est_sowing_conditions_v1_2014_24.xlsx", 
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
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))



### sum of rainfall from '10Nov' to sowing date

names(Est_sowing_conditions_v1_2014_24)
str(Est_sowing_conditions_v1_2014_24)

Est_sowing_conditions_v1_2014_24 <- Est_sowing_conditions_v1_2014_24 %>%
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
names(Est_sowing_conditions_v1_2014_24)


ESW_at_sowing <- Est_sowing_conditions_v1_2014_24 %>%
  filter(Wheat.Phenology.CurrentStageName == "Sowing") %>%
  select(
    `year`,
    `Wheat.Phenology.CurrentStageName`,
    `date`,
    
    `Sum_InitialValuesMM`,
    `SumESW`,
    `Sum_MM`,
    `Sum(Soil.SoilWater.PAWmm)`,
    
    `Soil.Water.MM(1)`,
    `Soil.Water.MM(2)`,
    `Soil.Water.MM(3)` ,
    `Soil.Water.MM(4)`,
    `Soil.Water.MM(5)`,
    `Soil.Water.MM(6)`,
    `Soil.Water.MM(7)`,
    
    `Soil.Physical.Thickness(1)`,
    `Soil.Physical.Thickness(2)`  ,
    `Soil.Physical.Thickness(3)` ,     
    `Soil.Physical.Thickness(4)` ,
    `Soil.Physical.Thickness(5)` ,
    `Soil.Physical.Thickness(6)` ,     
    `Soil.Physical.Thickness(7)` 
  )


## Define the GS period and assign season type ---------------------------------

Day_start_GS_rainfall <- 1
Month_start_GS_rainfall <- 4


Day_end_GS_rainfall <- 1
Month_end_GS_rainfall <- 11

#File start date and end date
paste("Start date in file is: ",
      min(Est_sowing_conditions_v1_2014_24$date),
      ". End date in file is: ",
      max(Est_sowing_conditions_v1_2014_24$date))

GS_defined_as <- paste0(
  Day_start_GS_rainfall, "/", Month_start_GS_rainfall,
  " to ", Day_end_GS_rainfall, "/", Month_end_GS_rainfall)

str(Est_sowing_conditions_v1_2014_24)

# Assign season type to climate data

Est_sowing_conditions_v1_2014_24 <- Est_sowing_conditions_v1_2014_24 %>% mutate(
  start_end_GS_date = case_when(
    month == Month_start_GS_rainfall & day_of_month == Day_start_GS_rainfall ~ "start_gs",
    month == Month_end_GS_rainfall & day_of_month == Day_end_GS_rainfall ~ "end_gs",
    TRUE ~ NA))

# Fill the blanks

Est_sowing_conditions_v1_2014_24 <- Est_sowing_conditions_v1_2014_24 %>% fill(start_end_GS_date) %>%
  mutate(
    season = case_when(
      start_end_GS_date == "start_gs" ~ "gs",
      TRUE ~ "summer"))

Est_sowing_conditions_v1_2014_24 <- Est_sowing_conditions_v1_2014_24 %>% select(-start_end_GS_date)
str(Est_sowing_conditions_v1_2014_24)

### what are the sowing dates for the xx years
sow_dates_by_year <- Est_sowing_conditions_v1_2014_24 %>% 
filter (Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  select(year, date,  Wheat.Phenology.CurrentStageName,  SumESW,Sum_MM )

list_sow_dates <- sow_dates_by_year[,2]
list_sow_dates

### This is super messing - but I just want a few dates for each year


summer_rain_input_days <- Est_sowing_conditions_v1_2014_24 %>% 
  filter(between(date, as.Date('2014-11-01'), as.Date('2015-04-01'))| #2014
           between(date, as.Date('2015-11-01'), as.Date('2016-04-01'))| #2015
           between(date, as.Date('2016-11-01'), as.Date('2017-04-01'))| #2016
           between(date, as.Date('2017-11-01'), as.Date('2018-04-01'))| #2017
           between(date, as.Date('2018-11-01'), as.Date('2019-04-01'))| #2018
           between(date, as.Date('2019-11-01'), as.Date('2020-04-01'))| #2019
           between(date, as.Date('2020-11-01'), as.Date('2021-04-01'))| #2020
           between(date, as.Date('2021-11-01'), as.Date('2022-04-01'))| #2021
           between(date, as.Date('2023-11-01'), as.Date('2024-04-01')) #2024
         )#small to big dates



# summer_rain1961 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)  
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1961-11-01'), 
#                  as.Date('1962-04-01'))) #small to big dates
# summer_rain1962 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1962-11-01'), 
#                  as.Date('1963-04-01'))) #small to big dates
# summer_rain1963 <- sum(summer_rain$Weather.Rain, na.rm = TRUE)
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1963-11-01'), 
#                  as.Date('1964-04-01'))) #small to big dates
# summer_rain1964 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1964-11-01'), 
#                  as.Date('1965-04-01'))) #small to big dates
# summer_rain1965 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1965-11-01'), 
#                  as.Date('1966-04-01'))) #small to big dates
# summer_rain1966 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1966-11-01'), 
#                  as.Date('1967-04-01'))) #small to big dates
# summer_rain1967 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1967-11-01'), 
#                  as.Date('1968-04-01'))) #small to big dates
# summer_rain1968 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1968-11-01'), 
#                  as.Date('1969-04-01'))) #small to big dates
# summer_rain1969 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1969-11-01'), 
#                  as.Date('1970-04-01'))) #small to big dates
# summer_rain1970 <- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1970-11-01'), 
#                  as.Date('1971-04-01'))) #small to big dates
# summer_rain1971<- sum(summer_rain$Weather.Rain, na.rm = TRUE)
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1971-11-01'), 
#                  as.Date('1972-04-01'))) #small to big dates
# summer_rain1972<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1972-11-01'), 
#                  as.Date('1973-04-01'))) #small to big dates
# summer_rain1973<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1973-11-01'), 
#                  as.Date('1974-04-01'))) #small to big dates
# summer_rain1974<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 
# 
# summer_rain <- Est_sowing_conditions_v1 %>% 
#   filter(between(date, 
#                  as.Date('1974-11-01'), 
#                  as.Date('1975-04-01'))) #small to big dates
# summer_rain1975<- sum(summer_rain$Weather.Rain, na.rm = TRUE) 


#year = seq(as.Date("1961/1/1"), as.Date("1975/1/1"), by = "year")
year <- seq(1961,1975)
str(summer_rain_input_days) 

sum_summer_rain_by_yr <- summer_rain_input_days %>% 
  group_by(year) %>% 
  summarise(
    sum_summer_rainfall = sum(Weather.Rain, na.rm = TRUE)
  )


ESW_at_sowing
sum_summer_rain_by_yr


soil_water_summer_rain <- left_join(sum_summer_rain_by_yr, ESW_at_sowing)
soil_water_summer_rain

#######################################################################

# Add a new variable soil water / depth
names(soil_water_summer_rain)

soil_water_summer_rain <- soil_water_summer_rain %>% 
  mutate(Soil_mm_depth_1 = `Soil.Water.MM(1)`/`Soil.Physical.Thickness(1)`,
         Soil_mm_depth_2 = `Soil.Water.MM(2)`/`Soil.Physical.Thickness(2)`,
         Soil_mm_depth_3 = `Soil.Water.MM(3)`/`Soil.Physical.Thickness(3)`,
         Soil_mm_depth_4 = `Soil.Water.MM(4)`/`Soil.Physical.Thickness(4)`,
         Soil_mm_depth_5 = `Soil.Water.MM(5)`/`Soil.Physical.Thickness(5)`,
         Soil_mm_depth_6 = `Soil.Water.MM(6)`/`Soil.Physical.Thickness(6)`,
         Soil_mm_depth_7 = `Soil.Water.MM(7)`/`Soil.Physical.Thickness(7)`,
         sum_soil_mm_depth = (Soil_mm_depth_1+
           Soil_mm_depth_2+
           Soil_mm_depth_3+
           Soil_mm_depth_4+
           Soil_mm_depth_5+
           Soil_mm_depth_6+
           Soil_mm_depth_7)
         )
  


inital_Soil_water_values <- unique(soil_water_summer_rain$Sum_InitialValuesMM)



str(soil_water_summer_rain)

plot_summer_rain <- soil_water_summer_rain %>% 
  ggplot(aes(x = year, sum_summer_rainfall))+
  
   geom_line()+
   annotate("text", x =2016, y = 40, label = "Sum summer rainfall", colour = "black")+
  
   geom_line(aes(y= SumESW), colour= "blue")+
   annotate("text", x =2016, y = 115, label = "Sum APSIM ESW at sowing", colour = "blue")+
   
   geom_line(aes(y= Sum_MM), colour= "darkolivegreen3")+
   annotate("text", x =2016, y = 210, label = "Sum APSIM MM at sowing", colour = "darkolivegreen3")+
  
   # geom_line(aes(y= sum_soil_mm_depth), colour= "darkolivegreen")+
   # annotate("text", x =2016, y = 2, label = "APSIM MM / dpeth at sowing  sum for profile", colour = "darkolivegreen")+
   
    theme_classic()+
  
  theme(legend.position = "none")+
  labs(title = "Summer rainfall and APSIM soil water ",
       subtitle = "Station Lock. Number: 18046. Years 2014-2024",
       x = "",
       y = "",
       caption = paste0("Inital APSIM soil water value MM", inital_Soil_water_values, ". 
                        Also note: Sum PAW and ESW are same values"))
plot_summer_rain

str(soil_water_summer_rain)
plot_soil_mm_div_thickness_sum <- soil_water_summer_rain %>% 
  ggplot(aes(x = year, sum_soil_mm_depth))+
  geom_line()
plot_soil_mm_div_thickness_sum
################################################################################

                                            
ggsave(plot = plot_summer_rain,
       filename ="X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/est_starting_soil_conditions/Results/plot_summer_rain_APSIM_soil_water2014.png",
       width = 20, height = 12, units = "cm")

write_csv(soil_water_summer_rain, 
          "X:/Riskwi$e/Dry_sowing/Lock/Dry_sowing/version5/est_starting_soil_conditions/Results/summer_rain_APSIM_soil_water2014.csv" )
