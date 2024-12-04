# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data Import --------------------------------------------------------------



Trial_setup_outputs <- read_excel("X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Dookie_trial_setup_outputs.xlsx", 
                                  sheet = "Treatments_Jackie", skip = 3)
names(Trial_setup_outputs)
str(Trial_setup_outputs)
unique(Trial_setup_outputs$`Jax_ID`)

### Match the treatment names to the simulation ----

Trial_setup_outputs <- Trial_setup_outputs %>% mutate(
  Treatment = case_when(
    `Jax_ID` == "01_Nil" ~ "Control",
    `Jax_ID` == "02_National Average" ~ "National_Av",
    `Jax_ID` == "03_Replacement + Protein" ~ "Replacment",
    
    `Jax_ID` == "04_YP Lite Decile 2-3" ~ "YP_Decile2_3",
    `Jax_ID` == "05_YP Lite Decile 5" ~ "YP_Decile5",
    `Jax_ID` == "06_YP Lite Decile 7-8" ~ "YP_Decile7_8",
    
    `Jax_ID` == "07_YP Lite BOM" ~ "YP_BOM",
    
    `Jax_ID` == "08_N Bank Conservative (NB250)" ~ "Nbank_Conservative",
    `Jax_ID` == "09_N Bank Optimal Profit (NB275)" ~ "Nbank_Optimum_Profit",
    `Jax_ID` == "10_N Bank Optimal Yield (NB300)" ~ "Nbank_Optimum_Yield"
  )
)

unique(Trial_setup_outputs$Treatment)


### remove the empty rows ----

Trial_setup_outputs <- Trial_setup_outputs %>% filter(!is.na(Treatment))



## Variable 1 ---------------------------------------------------------------------
##### 1.select the varibale clms ------------------------------------------------
Biomass_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,`Biomass flowering (t/ha)`,`Biomass harvest (t/ha)`,
         `Biomass harvest date`)  

##### 2. rename the columns
Biomass_DF <- Biomass_DF %>% rename(
  Biomass_at_harvest = "Biomass harvest (t/ha)",
  Biomass_at_flowering = "Biomass flowering (t/ha)")

##### 3.Make the varibale clm data long -----------------------------------------

names(Biomass_DF)
Biomass_DF_long <- pivot_longer(
    Biomass_DF, 
    cols = starts_with("Biomass_at"), 
    names_to = "Timing", 
    values_to = "Biomass"
  )
Biomass_DF_long

##### 4.remove the missing data ------------------------------------------------
Biomass_DF_long <- Biomass_DF_long %>%  filter(!is.na(Biomass))
Biomass_DF_long <- Biomass_DF_long %>%  rename (Date = `Biomass harvest date`)
Biomass <- Biomass_DF_long %>%  select(Treatment, Date, Biomass)

##### 5.tidy up df -------------------------------------------------------------

rm (Biomass_DF_long, Biomass_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Biomass , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/Biomass_Trial.csv", row.names = FALSE )




## Variable 2 ---------------------------------------------------------------------
##### 1.select the varibale clms ------------------------------------------------
names(Trial_setup_outputs)

SoilWater_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,'Sowing total water (mm) - soil',
         'Sowing date')  

##### 2. rename the columns
SoilWater_DF <- SoilWater_DF %>% rename(
  Soil_water_at_sowing = "Sowing total water (mm) - soil",
  Date = 'Sowing date')
  

##### 3.Make the varibale clm data long -----------------------------------------

names(SoilWater_DF)
SoilWater_DF_long <- pivot_longer(
  SoilWater_DF, 
  cols = starts_with("Soil_water_at"), 
  names_to = "Timing", 
  values_to = "Soil_water"
)
SoilWater_DF_long

##### 4.remove the missing data ------------------------------------------------
Biomass_DF_long <- Biomass_DF_long %>%  filter(!is.na(Biomass))
Biomass_DF_long <- Biomass_DF_long %>%  rename (Date = `Biomass harvest date`)
Biomass <- Biomass_DF_long %>%  select(Treatment, Date, Biomass)

##### 5.tidy up df -------------------------------------------------------------

rm (Biomass_DF_long, Biomass_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Biomass , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/Biomass_Trial.csv", row.names = FALSE )


Soil_mineral_N_sowing =`Amount of total mineral N at sowing (kg/ha) -Soil`,