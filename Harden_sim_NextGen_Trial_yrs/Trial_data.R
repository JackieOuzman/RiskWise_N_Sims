# This file is for importing Trial data.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# Trial data Import --------------------------------------------------------------



Trial_setup_outputs <- read_excel("X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_trial_setup_outputs.xlsx", 
                                  sheet = "Summary of Annual plot for sim", skip = 6)
names(Trial_setup_outputs)
Trial_setup_outputs <- Trial_setup_outputs %>% rename("Treatment description" = "!Treatment" )

str(Trial_setup_outputs)
unique(Trial_setup_outputs$`Jax_ID`)
unique(Trial_setup_outputs$`Treatment description`)


### Match the treatment names to the simulation ----


Trial_setup_outputs <- Trial_setup_outputs %>% mutate(
     Treatment = case_when(
       
       `Treatment description` == "01_Nil" ~                       "Control",
       `Treatment description` == "03_National_Average" ~          "National_Av",
       `Treatment description` == "04_100kgMaint" ~                "Maint_100",
       `Treatment description` == "05_125kgMaint" ~                "Maint_125",
       `Treatment description` == "06_150kgMaint" ~                "Maint_150",
       `Treatment description` == "07_100%YP" ~                    "YP_100",
       `Treatment description` == "08_75%YP" ~                     "YP_75",
       `Treatment description` == "09_50%YP" ~                     "YP_50",
       `Treatment description` == "10_25%YP" ~                     "YP_25",
       `Treatment description` == "02_Replacement" ~         "Replacment"
    )
)


unique(Trial_setup_outputs$Treatment)


### remove the empty rows ----

Trial_setup_outputs <- Trial_setup_outputs %>% filter(!is.na(Treatment))

names(Trial_setup_outputs)

## variable 1 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
Biomass_DF <- Trial_setup_outputs %>%
  select(
    Year,
    Treatment ,
    `Dry matter at Anthesis` ,
    `Dry matter at Harvest` ,
    `Plant cut date Anthesis`,
    `Plant cut date harvest`)

##### 2. rename the columns
Biomass_DF <- Biomass_DF %>% rename(
  Biomass_at_harvest =  `Dry matter at Harvest`,
  Biomass_at_anthesis = `Dry matter at Anthesis`,
  Date_anthesis = `Plant cut date Anthesis`,
  Date_harvest = `Plant cut date harvest`
)





##### 3.Make the variable clm data long -----------------------------------------

names(Biomass_DF)
Biomass_DF_long <- pivot_longer(
    Biomass_DF, 
    cols = starts_with("Biomass_at"), 
    names_to = "variable", 
    values_to = "Value"
  )
Biomass_DF_long

##### 3a.Make the date clm data long -----------------------------------------

names(Biomass_DF)
Biomass_DF_long2 <- pivot_longer(
  Biomass_DF_long, 
  cols = starts_with("Date_"), 
  names_to = "variable2", 
  values_to = "Date"
)
Biomass_DF_long2

#remove the duplicated rows
Biomass_DF_long3 <- Biomass_DF_long2 %>% 
  filter(variable ==  "Biomass_at_anthesis" & variable2 ==   "Date_anthesis"  |
  variable ==  "Biomass_at_harvest" & variable2 ==   "Date_harvest" ) %>% 
  select(- variable2 )

Biomass_DF_long3

##### 4.remove the missing data ------------------------------------------------
Biomass_DF_long3 <- Biomass_DF_long3 %>%  filter(!is.na(Value))
#Biomass_DF_long3 <- Biomass_DF_long3 %>%  rename (Date = Date_harvest)
Biomass <- Biomass_DF_long3 %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (Biomass_DF_long, Biomass_DF, Biomass_DF_long2, Biomass_DF_long3)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Biomass , "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/Biomass_Trial.csv", row.names = FALSE )




## variable 2 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

SoilWater_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,
         #'Sowing total water (mm) - soil',
         'Sowing date')  

##### 2. rename the columns No water in DF 
SoilWater_DF <- SoilWater_DF %>% mutate(
  Soil_water_at_sowing = 0.885 , #sum of inital water SW
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
SoilWater_DF_long <- SoilWater_DF_long %>%  filter(!is.na(Value))

Soil_water <- SoilWater_DF_long %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (SoilWater_DF_long, SoilWater_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Soil_water , "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/SoilWater_Trial.csv", row.names = FALSE )



## variable 3 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

  

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

##### 6.write variable df -------------------------------------------------------------
write.csv(SoilNitrogen , "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/SoilNitrogen_Trial.csv", row.names = FALSE )



## variable 4 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

                                                

Yield_DF <- Trial_setup_outputs %>% 
  select(Year, Treatment ,"Harvest Date" ,
         `Yield (t/ha)`
         #`Grain yield (machine) (area corrected) (t/ha)`  #this clm is empty in Curyo DB
         )  

##### 2. rename the columns
Yield_DF <- Yield_DF %>% rename(
  Yield =   `Yield (t/ha)`,
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


Yield <- Yield_DF_long %>%  select(Treatment, Date, Value)

##### 5.tidy up df -------------------------------------------------------------

rm (Yield_DF_long, Yield_DF)

##### 6.write varibale df -------------------------------------------------------------
write.csv(Yield , "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/Yield_Trial.csv", row.names = FALSE )



## variable 5 ---------------------------------------------------------------------
##### 1.select the variable clms ------------------------------------------------
names(Trial_setup_outputs)

# `Grain yield (machine) (area corrected) (t/ha)` 

#`Sowing fertiliser N rate (kg/ha)`
#`Topdress fertiliser 1 date`
#"Total N applied at sowing and inseason" 

Yield_Response_N_DF <- Trial_setup_outputs %>% 
  #mutate(
    #`Total N applied at sowing and inseason` = 'Total N applied at sowing and inseason' %>% 
      #`Sowing fertiliser N rate (kg/ha)`+`Topdress fertiliser 1 date`+ `Topdress fertiliser 2 N rate (kg/ha)`) %>% 
  select( Year, Treatment ,"Harvest Date" , 
         `Yield (t/ha)`,
         #`Grain yield (machine) (area corrected) (t/ha)` ,
         `Total N applied at sowing and inseason`)  

##### 2. rename the columns
Yield_Response_N_DF <- Yield_Response_N_DF %>% rename(
  #Yield = `Grain yield (machine) (area corrected) (t/ha)`,
  Yield = `Yield (t/ha)`,
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
write.csv(Yield_Response_N_DF , "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/Yield_Response_N_Trial.csv", row.names = FALSE )





