
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# SIM and Trial data --------------------------------------------------------------

merged_files <- read_csv(paste0(path, "merge_sim_trial.csv"))




# Make sure treatments are matching ---------------------------------------

#Trial <- Trial %>% filter(Treatment == "Control")





# Plot --------------------------------------------------------------------
Location_of_Sims<- "//FSSA2-ADL/clw-share1/mallee_mod/Riskwi$e/Bute/2_Sims_post_Sep2024/"
NextG_location <- "2_NextGen/Bute_5_Rotation_N_bank.apsimx"
Classic_Location <- "1_Classic/Bute_Rotation/4_Bute_rotation.apsim"


## Kirsten order of plotting
#1. Phenology (we don't have this site)
#2. Biomass (we don't have this at site)
#3. Soilwater (we don't have this at site, only initial)
#4. Soil NO3+NH4 (Yes) - Soil_mineral_N_sowing
#5. yield (Yes)
#6. response curves


str(merged_files)

#4. Soil_mineral_N_sowing 
Plot_4 <- merged_files %>% 
  ggplot(aes(x = as.factor(Year), y = Soil_mineral_N_sowing , colour =Source ))+
  geom_point(size = 3)+
  scale_color_manual(values=c("blue", "purple", "black"))+
  theme_bw()+
  labs(title = "Soil mineral N at sowing. Bute Sims vs trial data",
       subtitle = "No modifcation to organic matter",
       colour = "",
       x= "Year",
       y = "NO3 + NH4 kg/ha",
       caption = paste0("Location of Sims: ", Location_of_Sims, '\n',NextG_location, '\n',Classic_Location))+
  theme(plot.caption = element_text(hjust = 0))



#5. yield 
Plot_5 <- merged_files %>% 
  ggplot(aes(x = as.factor(Year), y = Yield , colour =Source ))+
  geom_point(size = 3)+
  scale_color_manual(values=c("blue", "purple", "black"))+
  theme_bw()+
  labs(title = "Yield. Bute Sims vs trial data",
       subtitle = "No modifcation to organic matter",
       colour = "",
       x= "Year",
       y = "Yield t/ha",
       caption = paste0("Location of Sims: ", Location_of_Sims, '\n',NextG_location, '\n',Classic_Location))+
  theme(plot.caption = element_text(hjust = 0))



ggsave(plot = Plot_4,filename =  paste0(path,"Soil mineral N at sowing.png"), width = 20, height = 12, units = "cm")
ggsave(plot = Plot_5, filename =  paste0(path,"Bute_plot5Yld.png"), width = 20, height = 12, units = "cm")
