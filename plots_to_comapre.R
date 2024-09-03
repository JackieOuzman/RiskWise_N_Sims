
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

Calssic <- read_csv(paste0(path, "APSIM_Classic_Rotations.csv"))
NextG <- read_csv(paste0(path, "APSIM_NextGen.csv"))
Trial <- read_csv(paste0(path, "APSIM_Trial_.csv"))

# Check it matches --------------------------------------------------------------

str(Calssic)
names(Calssic)
names(NextG) #missing NH4 at sowing
names(Trial)



# Make sure treatments are matching ---------------------------------------

Trial <- Trial %>% filter(Treatment == "Control")




# Join the 2 data set together --------------------------------------------

#format so they are all the same data type


Trial <- Trial %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
NextG <- NextG %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))
Calssic <- Calssic %>%
  mutate(across(-c(Source, Treatment, Crop, Cultivar ), as.numeric))


# Year              
# #Source     #chr      
# #Treatment  #chr      
# InCropFert        
# #Crop       #chr      
# #Cultivar   #chr     
# soil_water_start  
# soil_water_sowing 
# soil_water_harvest
# soil_NO3_start    
# soil_N03_sowing   
# soil_NO3_harvest  
# soil_NH4_start    
# soil_NH4_sowing   
# soil_NH4_harvest  
# Biomass           
# Yield             
# InCropRain        



merge_sim_trial <- bind_rows(Calssic, NextG, Trial)

write.csv(merge_sim_trial , 
          "X:/Riskwi$e/Bute/2_Sims_post_Sep2024/To_compare_etc/merge_sim_trial.csv", row.names = FALSE )



# Plot --------------------------------------------------------------------
Location_of_Sims<- "//FSSA2-ADL/clw-share1/mallee_mod/Riskwi$e/Bute/2_Sims_post_Sep2024/"
NextG_location <- "2_NextGen/Bute_5_Rotation_N_bank.apsimx"
Classic_Location <- "1_Classic/Bute_Rotation/4_Bute_rotation.apsim"


## Kirsten order of plotting
#1. Phenology (we don't have this site)
#2. Biomass (we don't have this at site)
#3. Soilwater (we don't have this at site, only initial)
#4. Soil N (we don't have this at site, only initial - I think check)
#5. yield ()
#6. response curves



merge_sim_trial %>% 
  ggplot(aes(x = as.factor(Year), y = Yield , colour =Source ))+
  geom_point(size = 3)+
  scale_color_manual(values=c("blue", "purple", "black"))+
  theme_bw()+
  labs(title = "Bute Sims vs trial data",
       colour = "",
       x= "Year",
       y = "Yield t/ha",
       caption = paste0("Location of Sims: ", Location_of_Sims, '\n',NextG_location, '\n',Classic_Location))+
  theme(plot.caption = element_text(hjust = 0))
