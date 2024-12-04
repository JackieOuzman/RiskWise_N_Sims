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



## variable 1 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
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
    names_to = "variable", 
    values_to = "Value"
  )
Biomass_DF_long

##### 4.remove the missing data ------------------------------------------------
Biomass_DF_long <- Biomass_DF_long %>%  filter(!is.na(Value))
Biomass_DF_long <- Biomass_DF_long %>%  rename (Date = `Biomass harvest date`)
Biomass <- Biomass_DF_long %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (Biomass_DF_long, Biomass_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Biomass , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/Biomass_Trial.csv", row.names = FALSE )




## variable 2 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
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
  names_to = "variable", 
  values_to = "Value"
)
SoilWater_DF_long

##### 4.remove the missing data ------------------------------------------------
#SoilWater_DF_long <- SoilWater_DF_long %>%  filter(!is.na(Value))

Soil_water <- SoilWater_DF_long %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (SoilWater_DF_long, SoilWater_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Soil_water , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/SoilWater_Trial.csv", row.names = FALSE )



## variable 3 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

# "Amount of NO3 at sowing (kg/ha) - Soil"                                                     
# "Amount of NH4 at sowing (kg/ha) - Soil"                                                     
# "Amount of total mineral N at sowing (kg/ha) -Soil"   

SoilNitrogen_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,"Sowing date" , 
         "Amount of NO3 at sowing (kg/ha) - Soil" ,
         "Amount of NH4 at sowing (kg/ha) - Soil"   ,
         "Amount of total mineral N at sowing (kg/ha) -Soil"
         )  

##### 2. rename the columns
SoilNitrogen_DF <- SoilNitrogen_DF %>% rename(
  Soil_NO3_at_sowing = "Amount of NO3 at sowing (kg/ha) - Soil",
  Soil_NH4_at_sowing = "Amount of NH4 at sowing (kg/ha) - Soil",
  Soil_TotalN_at_sowing = "Amount of total mineral N at sowing (kg/ha) -Soil",
  Date = "Sowing date")


##### 3.Make the variable clm data long -----------------------------------------

names(SoilNitrogen_DF)
SoilNitrogen_DF_long <- pivot_longer(
  SoilNitrogen_DF, 
  cols = starts_with("Soil_"), 
  names_to = "variable", 
  values_to = "Value"
)
SoilNitrogen_DF_long

##### 4.remove the missing data ------------------------------------------------
SoilNitrogen_DF_long <- SoilNitrogen_DF_long %>%  filter(!is.na(Value))

SoilNitrogen <- SoilNitrogen_DF_long %>%  select(Treatment, Date, Value, variable)

##### 5.tidy up df -------------------------------------------------------------

rm (SoilNitrogen_DF_long, SoilNitrogen_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(SoilNitrogen , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/SoilNitrogen_Trial.csv", row.names = FALSE )



## variable 4 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

# "Yield (t/ha)"                                                 

Yield_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,"Harvest Date" , 
         "Yield (t/ha)"    )  

##### 2. rename the columns
Yield_DF <- Yield_DF %>% rename(
  Yield = "Yield (t/ha)",
  Date = "Harvest Date")


##### 3.Make the variable clm data long -----------------------------------------

names(Yield_DF)
# Yield_DF_long <- pivot_longer(
#   Yield_DF, 
#   cols = starts_with("Yield_"), 
#   names_to = "variable", 
#   values_to = "Value"
# )
Yield_DF_long <- Yield_DF %>%  rename(
  Value = Yield) %>% 
  mutate(variable = "yield")

##### 4.remove the missing data ------------------------------------------------
Yield_DF_long <- Yield_DF_long %>%  filter(!is.na(Value))
Yield_DF_long <- Yield_DF_long %>%  filter(Date > 2017-01-01) # missing dates

Yield <- Yield_DF_long %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (Yield_DF_long, Yield_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Yield , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/Yield_Trial.csv", row.names = FALSE )



## variable 5 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

# "Yield (t/ha)" 
#"Total N applied at sowing and inseason" 

Yield_Response_N_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,"Harvest Date" , 
         "Yield (t/ha)" ,
         "Total N applied at sowing and inseason")  

##### 2. rename the columns
Yield_Response_N_DF <- Yield_Response_N_DF %>% rename(
  Yield = "Yield (t/ha)",
  Date = "Harvest Date",
  N_applied = "Total N applied at sowing and inseason"
  )


##### 3.Make the variable clm data long -----------------------------------------

names(Yield_Response_N_DF)



##### 4.remove the missing data ------------------------------------------------
Yield_Response_N_DF <- Yield_Response_N_DF %>%  filter(!is.na(Yield))
Yield_Response_N_DF <- Yield_Response_N_DF %>%  filter(!is.na(N_applied))
#Yield_Response_N_DF <- Yield_Response_N_DF %>%  filter(Date > 2017-01-01) # missing dates



##### 5.tidy up df -------------------------------------------------------------



##### 6.write variable df -------------------------------------------------------------
write.csv(Yield_Response_N_DF , "X:/Riskwi$e/Dookie/3_Sims_post_Nov2024/Results/Yield_Response_N_Trial.csv", row.names = FALSE )





