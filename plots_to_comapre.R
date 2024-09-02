
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
# SIM data --------------------------------------------------------------

sim_output <- read_csv("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/APSIM_Classic_output_long.csv")

# Trial data --------------------------------------------------------------



Bute_trial_setup_outputs <- read_excel("X:/Riskwi$e/Bute/2_Sims_post_Sep2024/Bute_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 2)
str(Bute_trial_setup_outputs)

Bute_trial_setup_outputs  <- Bute_trial_setup_outputs %>% 
  select(Year , 
         treatment = System ,
         Crop_type = 'Crop type' ,
         "Variety_trial"=  Variety ,
         "Soil_N_sowing" = `Soil mineral N (kg/ha) - prior to sowing`,
         "N_applied_sowing" = `Amount of N applied at sowing kg/ha` ,
         "N_applied_inSeason_1" = `1. Top dressed N (kg/ha)` ,
         "N_applied_inSeason_2" =  `2. Top dressed N (kg/ha)` ,
         "Yield_t_ha" = `Yield (t/ha)`  
         
  )
Bute_trial_setup_outputs  <- Bute_trial_setup_outputs %>% 
  mutate(for_join = paste0(Year ,"_", treatment))




# Control treatment -------------------------------------------------------

#Yield only


Bute_trial_setup_outputs_control_yld <- Bute_trial_setup_outputs %>% 
  filter(treatment == "Control")
str(Bute_trial_setup_outputs_control_yld)




Bute_trial_setup_outputs_control_yld<- Bute_trial_setup_outputs_control_yld %>% 
  select(for_join,Yield_t_ha)

Bute_trial_setup_outputs_control_yld
sim_output_yld <- sim_output %>% filter(variable == "yield" & Event == "harvest") %>% 
  select(value ,treatment ,for_join, Year ) %>% 
  mutate(value = value/1000)

sim_output_yld
Bute_trial_setup_outputs_control_yld

join<- left_join(sim_output_yld, Bute_trial_setup_outputs_control_yld)

join_long <- join %>%  pivot_longer(cols= c("value" , "Yield_t_ha"),
                                    names_to = "source",
                                    values_to = "yield_t_ha")

join_long <- join_long %>% mutate(source = case_when(
  source  == "Yield_t_ha" ~ "Trial",
  source  == "value" ~ "Classic"
))
join_long



# Plot --------------------------------------------------------------------

join_long %>% 
  ggplot(aes(x = as.factor(Year), y = yield_t_ha, colour =source ))+
  geom_point()
