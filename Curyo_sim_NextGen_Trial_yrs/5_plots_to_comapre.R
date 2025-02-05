
# This file is for importing APISM sim files.

# install.packages("patchwork")

library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(patchwork)

# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------

merged_files_Daily <- read_csv(paste0(path, "APSIM_NextGen_Daily.csv"))
str(merged_files_Daily$Date)

trial_data <- read_csv(paste0(path, "merge_trial_harvest.csv"))
str(trial_data$Date)
# merged_files_harvest <- read_csv(paste0(path, "merge_sim_trial_harvest.csv"))
# str(merged_files_harvest$Year)


# Make sure treatments are matching ---------------------------------------


unique(merged_files_Daily$Treatment)
merged_files_Daily %>% filter(Treatment == "YP_100") %>% 
  summarise(max_biomass = max(Biomass),
            max_yld = max( Yield))

merged_files_Daily %>% filter(Treatment == "Maint_150") %>% 
  summarise(max_biomass = max(Biomass),
            max_yld = max( Yield))

unique(merged_files_Daily$Treatment)
unique(trial_data$Treatment)

# Adjustment for moisture correct to match what is in field trial info ---------

str(merged_files_Daily)

merged_files_Daily <- merged_files_Daily %>% 
  mutate(Yield_Moisture_corrected = case_when(
  Year == 2018 ~ Yield*12,
  Year == 2019 ~ Yield*12,
  Year == 2020 ~ Yield*12,
  Year == 2021 ~ Yield*12,
  Year == 2022 ~ Yield*12.5)) %>% 
    
  mutate(Biomass_Moisture_corrected = case_when(
    Year == 2018 ~ Biomass*12,
    Year == 2019 ~ Biomass*12,
    Year == 2020 ~ Biomass*12,
    Year == 2021 ~ Biomass*12,
    Year == 2022 ~ Biomass*12.5))

# Plot --------------------------------------------------------------------
Location_of_Sims<- "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/"
NextG_location <- "Curyo_5_Rotation_N_bank.apsimx"




## Kirsten order of plotting
#1. Phenology (we don't have this site)
#2. Biomass (we don't have this at site)
#3. Soilwater (we don't have this at site, only initial)
#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
#5. yield (Yes)
#6. response curves


str(merged_files_Daily)
str(trial_data)

# test <- merged_files_Daily %>% filter(Date == "2022-11-27") %>% 
#   filter(Treatment=="Control") %>% 
#   filter(Source == "NextGen")

#2. Biomass #Plot_2
plot2 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  ggplot(aes(x = (Date), y = Biomass_Moisture_corrected )) +
  geom_point(size = 2, colour = "blue") +
  theme_bw() +
  labs(
    title = "Biomass. Curyo Sims *12 and 12.5 (2022) ",
    #subtitle = "No modifcation to organic matter",
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location
     
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  trial_data %>% 
  filter(!is.na(Treatment)) %>% 
  geom_point(mapping = aes(x = Date, y = Biomass ),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot2



ggsave(plot = plot2,filename =  paste0(path,"Biomass.png"), width = 20, height = 12, units = "cm")


names(merged_files_Daily)

#3. Soilwater 
plot3 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = soil_water )) +
  geom_point(size = 1, colour = "blue") +
  theme_bw() +
  labs(
    title = "Soil Water. Curyo Sims ",
   
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location
     
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
plot3

ggsave(plot = plot3,filename =  paste0(path,"soilWater.png"), width = 20, height = 12, units = "cm")



#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
trial_data$Date

sowing_dates <-  trial_data %>%
  filter(  Date == "2018-10-10"|
           Date == "2019-11-15"|
           Date == "2020-11-21"|
           Date == "2021-11-25"|
           Date == "2022-12-07"  ) %>% 
  mutate(Date = case_when(
    Date == "2018-10-10" ~ "2018-05-14",
    Date == "2019-11-15" ~ "2019-04-29",
    Date == "2020-11-21" ~ "2020-05-16",
    Date == "2021-11-25" ~ "2021-05-14",
    Date == "2022-12-07" ~ "2022-05-19"
    
  ))

sowing_dates$Date <- as.Date(sowing_dates$Date)
str(sowing_dates$Date)
str(merged_files_Daily$Date)


names(trial_data)
plot4 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = soil_NO3 )) +
  geom_point(size = 1, colour = "blue") + 
  theme_bw() +
  labs(
    title = "Soil NO3 Curyo Sims ",
    #subtitle = "No modifcation to organic matter",
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location
      
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
 facet_wrap(.~Treatment)+
  sowing_dates %>%
  geom_point(mapping = aes(x = Date, y = Soil_mineral_N_sowing),
            colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot4
ggsave(plot = plot4,filename =  paste0(path,"soilNO3.png"), width = 20, height = 12, units = "cm")


names(merged_files_Daily)
names(trial_data)
plot5 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
 # filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Yield_Moisture_corrected )) +
  geom_point(size = 1, colour = "blue") +
  
  theme_bw() +
  labs(
    title = "Yield Curyo Sims *12 and 12.5 (2022) ",
   # subtitle = "No modifcation to organic matter",
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  trial_data %>%
  filter(!is.na(Treatment)) %>% 
  #filter(Date == "2022-12-07"|Date == "2023-10-27") %>%
  geom_point(mapping = aes(x = Date, y = Yield),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot5
ggsave(plot = plot5,filename =  paste0(path,"Yield.png"), width = 20, height = 12, units = "cm")



# Response curve ----------------------------------------------------------


Zadock_Stage90 <- merged_files_Daily %>% #just getting the max yield for each year and treatment
  group_by(Treatment, Year, Source) %>% 
  summarise(max_yld = max(Yield_Moisture_corrected))


# Make two data sets one for plotting one for joining to get N app --------


trial_data_plot <- trial_data %>% 
  filter(  Date == "2018-10-10"|
           Date == "2019-11-15"|
           Date == "2020-11-21"|
           Date == "2021-11-25"|
           Date == "2022-12-07") %>% #These are the harvest dates
  
  
  
  select( "Year" ,
          "Treatment",
          "InCropFert" ,
          "Date"  ,
          "Yield",
          "Source")

trial_data_join <- trial_data %>% 
  filter(   Date == "2018-10-10"|
            Date == "2019-11-15"|
            Date == "2020-11-21"|
            Date == "2021-11-25"|
            Date == "2022-12-07") %>% #These are the harvest dates
  select( "Year" ,
          "Treatment",
          "InCropFert" ,
          "Date"  )


Response_input <- left_join(Zadock_Stage90, trial_data_join, , by = join_by(Year,Treatment )) %>% 
  rename(Yield = max_yld)


Response_input <- bind_rows(Response_input,trial_data_plot )





N_Response <- Response_input %>% 
  ggplot(mapping = aes(x=InCropFert, y = Yield, colour = Source))+
  geom_point(size = 2)+
  #geom_line()+
  scale_color_manual(values = c("blue",  "black")) +
  theme_bw() +
  labs(
    title = "Response curve Curyo (yld*12)and 12.5 (2022)",
    colour = "",
    x = "InCropFert",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location
      
    )
  )+
  facet_wrap(.~Year)
N_Response

ggsave(plot = N_Response,filename =  paste0(path,"N_Response.png"), width = 20, height = 12, units = "cm")




# NStress Water Stress with Biomass formatted for comparison-----------------------------------------------------------------
sowing_dates <- data.frame(date = as.Date(c(
  "2018-05-14", "2019-04-29", "2020-05-16", "2021-05-14", "2022-05-19"
)))
sowing_dates

Fert_dates <- data.frame(date = as.Date(c(
  "2018-08-28", "2019-06-28","2020-07-23", "2021-07-23", "2022-07-08" )))
Fert_dates

Harvest_dates <- data.frame(date = as.Date(
  c(
    "2018-10-10",
    "2019-11-15",
    "2020-11-21",
    "2021-11-25",
    "2022-12-07"  
  )
))
Harvest_dates

# Big_rain_dates<- data.frame(date = as.Date(c("2022-08-09","2022-11-01","2023-04-15","2023-12-10")))
# Big_rain_dates






# Biomass formatted for comparison ----------------------------------------

Biomass_format <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Biomass_Moisture_corrected, colour = Source)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "Biomass. Curyo Sims ",
    colour = "",
    #x = "Year",
    y = "Biomass",
   
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  trial_data %>%
  filter(!is.na(Treatment)) %>% 
  geom_point(mapping = aes(x = Date, y = Biomass),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  
  geom_vline(xintercept = sowing_dates[1:5, 1], # 5 years of data select 5 rows
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:5, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:5, 1],
             color = "grey",
             lwd = 0.4)#+
  # geom_vline(xintercept = Big_rain_dates[1:2, 1],
  #          color = "lightblue",
  #          lwd = 0.4) 
  
Biomass_format


# NStress formatted for comparison ----------------------------------------
unique(merged_files_Daily$Source)




NStress_NextG <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  filter(Source == "NextGen") %>%
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = NSTress , colour = Source
  )) +
  geom_point(size = 1, colour = "purple") +
  #scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "",
    colour = "",
    #x = "Year",
    y = "N Stress - NextGen",
    
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_vline(xintercept = sowing_dates[1:5, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:5, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:5, 1],
             color = "grey",
              lwd = 0.4) #+
  # geom_vline(xintercept = Big_rain_dates[1:5, 1],
  #            color = "lightblue",
  #            lwd = 0.4) 
  



NStress_NextG








# WaterStress formatted for comparison -----------------------------------------------------------------

names(merged_files_Daily)




WaterStress_NextG <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  filter(Source == "NextGen") %>%
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = WaterStress , colour = Source)) +
  geom_point(colour = "purple") +
  scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "",
    colour = "",
    #x = "Year",
    y = "Water Stress - NextGen"
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  geom_vline(xintercept = sowing_dates[1:5, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:5, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:5, 1],
             color = "grey",
             lwd = 0.4) +
  # geom_vline(xintercept = Big_rain_dates[1:2, 1],
  #            color = "lightblue",
  #            lwd = 0.4) +
  theme(axis.title.x=element_blank())#,
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())



WaterStress_NextG




collated_plots1 <- Biomass_format /   NStress_NextG 
collated_plots2 <- Biomass_format /   WaterStress_NextG

collated_plots1
collated_plots2

ggsave(plot = collated_plots1,filename =  paste0(path,"NStress.png"), width = 20, height = 12, units = "cm")
ggsave(plot = collated_plots2,filename =  paste0(path,"WaterStress.png"), width = 20, height = 12, units = "cm")

