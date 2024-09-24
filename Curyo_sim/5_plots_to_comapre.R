
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

path <- "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------

merged_files_Daily <- read_csv(paste0(path, "merge_sim_Daily.csv"))
str(merged_files_Daily$Date)

trial_data <- read_csv(paste0(path, "merge_trial_harvest.csv"))
str(trial_data$Date)
# merged_files_harvest <- read_csv(paste0(path, "merge_sim_trial_harvest.csv"))
# str(merged_files_harvest$Year)


# Make sure treatments are matching ---------------------------------------







# Plot --------------------------------------------------------------------
Location_of_Sims<- "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/"
NextG_location <- "2_NextGen/Curyo_5_Rotation_N_bank.apsimx"
Classic_Location <- "1_Classic/Operation_schedule/3_operation schedule.apsim"


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
  #filter(!is.na(Treatment)) %>% 
  ggplot(aes(x = (Date), y = Biomass , colour = Source)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "purple")) +
  theme_bw() +
  labs(
    title = "Biomass. Curyo Sims ",
    #subtitle = "No modifcation to organic matter",
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location,
      '\n',
      Classic_Location
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  trial_data %>%
  geom_point(mapping = aes(x = Date, y = Biomass),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot2



ggsave(plot = plot2,filename =  paste0(path,"Biomass.png"), width = 20, height = 12, units = "cm")


names(merged_files_Daily)

#3. Soilwater 
plot3 <- merged_files_Daily %>%
  
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = soil_water , colour = Source)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("blue", "purple")) +
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
      NextG_location,
      '\n',
      Classic_Location
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
plot3
ggsave(plot = plot3,filename =  paste0(path,"soilWater.png"), width = 20, height = 12, units = "cm")



#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
sowing_dates <-  trial_data %>%
  filter(Date == "2022-12-07"|Date == "2023-10-27") %>% 
  mutate(Date = case_when(
    Date == "2022-12-07" ~ "2022-06-03",
    Date == "2023-10-27" ~ "2023-05-16"
  ))

sowing_dates$Date <- as.Date(sowing_dates$Date)
str(sowing_dates$Date)
str(merged_files_Daily$Date)


names(trial_data)
plot4 <- merged_files_Daily %>%
  
  filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = soil_NO3 , colour = Source)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("blue", "purple")) +
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
      NextG_location,
      '\n',
      Classic_Location
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
 facet_wrap(.~Treatment)+
  sowing_dates %>%
  #filter(Date == "2022-12-07"|Date == "2023-10-27") %>%
 geom_point(mapping = aes(x = Date, y = Soil_mineral_N_sowing),
            colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot4
ggsave(plot = plot4,filename =  paste0(path,"soilNO3.png"), width = 20, height = 12, units = "cm")


names(merged_files_Daily)
names(trial_data)
plot5 <- merged_files_Daily %>%
  
  filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Yield , colour = Source)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("blue", "purple")) +
  theme_bw() +
  labs(
    title = "Yield Curyo Sims ",
   # subtitle = "No modifcation to organic matter",
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location,
      '\n',
      Classic_Location
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  trial_data %>%
  filter(Date == "2022-12-07"|Date == "2023-10-27") %>%
  geom_point(mapping = aes(x = Date, y = Yield),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot5
ggsave(plot = plot5,filename =  paste0(path,"Yield.png"), width = 20, height = 12, units = "cm")



# Response curve ----------------------------------------------------------


Zadock_Stage90_2022 <- merged_files_Daily %>% #just getting the max yield for each year and treatment
  group_by(Treatment, Year, Source) %>% 
  summarise(max_yld = max(Yield))


# Make two data sets one for plotting one for joining to get N app --------


trial_data_plot <- trial_data %>% 
  filter(Date == "2022-12-07"|Date == "2023-10-27") %>% #These are the harvest dates
  select( "Year" ,
          "Treatment",
          "InCropFert" ,
          "Date"  ,
          "Yield",
          "Source")

trial_data_join <- trial_data %>% 
  filter(Date == "2022-12-07"|Date == "2023-10-27") %>% #These are the harvest dates
  select( "Year" ,
          "Treatment",
          "InCropFert" ,
          "Date"  )


Response_input <- left_join(Zadock_Stage90_2022, trial_data_join, , by = join_by(Year,Treatment )) %>% 
  rename(Yield = max_yld)


Response_input <- bind_rows(Response_input,trial_data_plot )





N_Response <- Response_input %>% 
  ggplot(mapping = aes(x=InCropFert, y = Yield, colour = Source))+
  geom_point(size = 2)+
  #geom_line()+
  scale_color_manual(values = c("blue", "purple", "black")) +
  theme_bw() +
  labs(
    title = "Response curve Curyo",
    colour = "",
    x = "InCropFert",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location,
      '\n',
      Classic_Location
    )
  )+
  facet_wrap(.~Year)
N_Response

ggsave(plot = N_Response,filename =  paste0(path,"N_Response.png"), width = 20, height = 12, units = "cm")




# NStress Water Stress with Biomass formatted for comparison-----------------------------------------------------------------
sowing_dates <- data.frame(date = as.Date(c("2022-06-03", "2023-05-16")))
sowing_dates

Fert_dates <- data.frame(date = as.Date(c("2022-07-24", "2023-07-07")))
Fert_dates

Harvest_dates <- data.frame(date = as.Date(c("2022-12-07", "2023-10-23")))
Harvest_dates

Big_rain_dates<- data.frame(date = as.Date(c("2022-08-09","2022-11-01","2023-04-15","2023-12-10")))
Big_rain_dates






# Biomass formatted for comparison ----------------------------------------

Biomass_format <- merged_files_Daily %>%
  filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Biomass , colour = Source)) +
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
  geom_point(mapping = aes(x = Date, y = Biomass),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  
  geom_vline(xintercept = sowing_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:2, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:2, 1],
             color = "grey",
             lwd = 0.4)+
  geom_vline(xintercept = Big_rain_dates[1:2, 1],
           color = "lightblue",
           lwd = 0.4) 
  
Biomass_format


# NStress formatted for comparison ----------------------------------------
unique(merged_files_Daily$Source)

NStress_Classic <- merged_files_Daily %>%
  filter(Source == "APISM_Classic_Rotation") %>%
  filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = NSTress , colour = Source
             )) +
  geom_point(size = 1, colour = "blue") +
  scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "",
    colour = "",
    #x = "Year",
    y = "N Stress - Classic",
   
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_vline(xintercept = sowing_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:2, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Big_rain_dates[1:2, 1],
             color = "lightblue",
             lwd = 0.4) 



NStress_Classic


NStress_NextG <- merged_files_Daily %>%
  filter(Source == "NextGen") %>%
  filter(zadok_stage >= 0) %>%
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
  geom_vline(xintercept = sowing_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:2, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Big_rain_dates[1:2, 1],
             color = "lightblue",
             lwd = 0.4) 
  



NStress_NextG








# WaterStress formatted for comparison -----------------------------------------------------------------

names(merged_files_Daily)


WaterStress_Classic <- merged_files_Daily %>%
  filter(Source == "APISM_Classic_Rotation") %>%
  filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = WaterStress , colour = Source)) +
  geom_point(colour = "blue") +
  scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "",
    colour = "",
    #x = "Year",
    y = "Water Stress - Classic"
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")+
  geom_vline(xintercept = sowing_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:2, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Big_rain_dates[1:2, 1],
             color = "lightblue",
             lwd = 0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



WaterStress_Classic

WaterStress_NextG <- merged_files_Daily %>%
  filter(Source == "NextGen") %>%
  filter(zadok_stage >= 0) %>%
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
  geom_vline(xintercept = sowing_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Fert_dates[1:2, 1],
             color = "lightgreen",
             lwd = 0.4) +
  geom_vline(xintercept = Harvest_dates[1:2, 1],
             color = "grey",
             lwd = 0.4) +
  geom_vline(xintercept = Big_rain_dates[1:2, 1],
             color = "lightblue",
             lwd = 0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



WaterStress_NextG




collated_plots1 <- Biomass_format / NStress_Classic/  NStress_NextG 
collated_plots2 <- Biomass_format / WaterStress_Classic/  WaterStress_NextG

collated_plots1
collated_plots2

ggsave(plot = collated_plots1,filename =  paste0(path,"NStress.png"), width = 20, height = 12, units = "cm")
ggsave(plot = collated_plots2,filename =  paste0(path,"WaterStress.png"), width = 20, height = 12, units = "cm")

