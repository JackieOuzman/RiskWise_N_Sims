
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

path <- "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/"


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

Biomass_Trial          <- read_csv(paste0(path, "Biomass_Trial.csv")) 
SoilWater_Trial        <- read_csv(paste0(path, "SoilWater_Trial.csv")) 
SoilNitrogen_Trial     <- read_csv(paste0(path, "SoilNitrogen_Trial.csv"))   
Yield_Trial            <- read_csv(paste0(path, "Yield_Trial.csv")) 
N_response             <- read_csv(paste0(path, "Yield_Response_N_Trial.csv")) 



# Make sure treatments are matching ---------------------------------------







# Plot --------------------------------------------------------------------
Location_of_Sims<- "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/"
NextG_location <- "Dookie_5_Rotation_N_bank.apsimx"




## Kirsten order of plotting
#1. Phenology (we don't have this site)
#2. Biomass (we don't have this at site)
#3. Soilwater (we don't have this at site, only initial)
#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
#5. yield (Yes)
#6. response curves


str(merged_files_Daily)
str(Biomass_Trial)


# test <- merged_files_Daily %>% filter(Date == "2022-11-27") %>% 
#   filter(Treatment=="Control") %>% 
#   filter(Source == "NextGen")

#2. Biomass #Plot_2
plot2 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  ggplot(aes(x = (Date), y = Biomass )) +
  geom_point(size = 2, colour = 'blue') +
  theme_bw() +
  labs(
    title = "Biomass. Dookie Sims ",
    subtitle = "No trial biomass data collected / found ",
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
  Biomass_Trial %>% 
   geom_point(mapping = aes(x = Date, y = Value),
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
    title = "Soil Water. Dookie Sims ",
   
    colour = "",
    x = "Year",
    y = "",
    caption = paste0(
      "Location of Sims: ",
      '\n',
      Location_of_Sims,
      '\n',
      NextG_location,
      '\n'
    )
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
plot3

ggsave(plot = plot3,filename =  paste0(path,"soilWater.png"), width = 20, height = 12, units = "cm")



#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
 unique(SoilNitrogen_Trial$variable)



names(trial_data)
plot4 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = soil_NO3 )) +
  geom_point(size = 1, colour = "blue") +
  theme_bw() +
  labs(
    title = "Soil NO3 Dookie Sims ",
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
  SoilNitrogen_Trial %>%
 filter(variable == "Soil_TotalN_at_sowing") %>% 
  geom_point(mapping = aes(x = Date, y = Value),
            colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot4
ggsave(plot = plot4,filename =  paste0(path,"soilNO3.png"), width = 20, height = 12, units = "cm")


names(merged_files_Daily)
names(trial_data)
plot5 <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
 # filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Yield )) +
  geom_point(size = 1, colour = "blue") +
  theme_bw() +
  labs(
    title = "Yield Dookie Sims ",
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
  Yield_Trial %>%
  geom_point(mapping = aes(x = Date, y = Value),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot5
ggsave(plot = plot5,filename =  paste0(path,"Yield.png"), width = 20, height = 12, units = "cm")



# Response curve ----------------------------------------------------------
names(merged_files_Daily)

Next_Gen_N_response <- merged_files_Daily %>% #just getting the max yield and incrop fert for each year and treatment
  group_by(Treatment, Year) %>% 
  summarise(yld = max(Yield),
           Incrop_fert = max(InCropFert))

str(N_response)



N_Response_plot <- Next_Gen_N_response %>% 
  ggplot(mapping = aes(x=Incrop_fert, y = yld))+
  geom_point(size = 2, colour = "blue")+
  theme_bw() +
  labs(
    title = "Response curve Dookie",
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
  facet_wrap(.~Year)+
  N_response %>%
  geom_point(mapping = aes(x = N_applied, y = Yield    ),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

N_Response_plot

ggsave(plot = N_Response,filename =  paste0(path,"N_Response.png"), width = 20, height = 12, units = "cm")




# NStress Water Stress with Biomass formatted for comparison-----------------------------------------------------------------

max(merged_files_Daily$Date)

sowing_dates <- data.frame(date = as.Date(c(
  "2022-04-15",
  "2023-05-04"
)))
sowing_dates

Fert_dates <- data.frame(date = as.Date(c(
  "2022-05-01", 
  "2022-06-28",
  "2023-08-02" )))
Fert_dates

Harvest_dates <- data.frame(date = as.Date(
  c(
    "2022-11-10",
    "2023-11-23"  )
))
Harvest_dates

# Big_rain_dates<- data.frame(date = as.Date(c("2022-08-09","2022-11-01","2023-04-15","2023-12-10")))
# Big_rain_dates






# Biomass formatted for comparison ----------------------------------------

Biomass_format <- merged_files_Daily %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
  ggplot(aes(x = (Date), y = Biomass , colour = Source)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "purple")) +
  theme_classic() +
  labs(
    #title = "Biomass. Dookie Sims ",
    colour = "",
    #x = "Year",
    y = "Biomass",
   
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Treatment, nrow = 1)+
  # trial_data %>% # no biomass data
  # geom_point(mapping = aes(x = Date, y = Biomass),
  #            colour = "black")+
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

NStress_Classic <- merged_files_Daily %>%
  filter(Source == "APISM_Classic_Operation_Sch") %>%
  filter(!is.na(Treatment)) %>% 
  #filter(zadok_stage >= 0) %>%
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



NStress_Classic


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


WaterStress_Classic <- merged_files_Daily %>%
  filter(Source == "APISM_Classic_Operation_Sch") %>%
  #filter(zadok_stage >= 0) %>%
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
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
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
  # geom_vline(xintercept = Big_rain_dates[1:5, 1],
  #            color = "lightblue",
  #            lwd = 0.4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



WaterStress_Classic

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




collated_plots1 <- Biomass_format / NStress_Classic/  NStress_NextG 
collated_plots2 <- Biomass_format / WaterStress_Classic/  WaterStress_NextG

collated_plots1
collated_plots2

ggsave(plot = collated_plots1,filename =  paste0(path,"NStress.png"), width = 20, height = 12, units = "cm")
ggsave(plot = collated_plots2,filename =  paste0(path,"WaterStress.png"), width = 20, height = 12, units = "cm")

