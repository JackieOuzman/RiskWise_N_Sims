
# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)


# File path for formatted outputs -----------------------------------------

path <- "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/"


list_sim_out_file <-
  list.files(
    path = path,
    pattern = ".csv",
    all.files = FALSE,
    full.names = FALSE
  )

list_sim_out_file

# Trial data from excel sheet--------------------------------------------------------------
Bute_trial_setup_outputs <- read_excel("X:/Riskwi$e/Bute/3_Sims_post_Nov2024/Bute_trial_setup_outputs.xlsx", 
                                       sheet = "Treatments_Jackie", skip = 2)
str(Bute_trial_setup_outputs)
#get the harvest dates
Harvest_Dates2022 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))

Harvest_Dates2023 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Harvest Date`) %>% 
  summarise(Harvest_date = max(`Harvest Date`))


Anthesis_Dates2022 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2022) %>% 
  select(`Anthesis date`) %>% 
  summarise(date = max(`Anthesis date`))


Anthesis_Dates2023 <- Bute_trial_setup_outputs %>% 
  filter(Year == 2023) %>% 
  select(`Anthesis date`) %>% 
  summarise(date = max(`Anthesis date`))

Harvest_Dates2022 <- Harvest_Dates2022$Harvest_date
Harvest_Dates2023 <- Harvest_Dates2023$Harvest_date
Anthesis_Dates2022 <- Anthesis_Dates2022$date
Anthesis_Dates2023 <- Anthesis_Dates2023$date



# Trial data with formatting in R --------------------------------------------------------------
Trial <- read_csv(paste0(path, "APSIM_Trial_.csv"))


# Stuff around getting DM Anthesis into its own dataset and rename biomass to match trial data-------------------

names(Trial)
Anthesis <- Trial %>% select(-Biomass)

Anthesis <- Anthesis %>% mutate(
  Date=case_when(
    Year == 2022 ~ Anthesis_Dates2022,
    Year == 2023 ~ Anthesis_Dates2023
  ))

Anthesis <- Anthesis %>% rename(Biomass = DM_Anthesis)





# Remove anthesis clm from trial data set and add a date clm ---------------------------------

Trial <- Trial %>% select(-DM_Anthesis)
Trial <- Trial %>% mutate(
  Date=case_when(
    Year == 2022 ~ Harvest_Dates2022,
    Year == 2023 ~ Harvest_Dates2023
  ))

names(Trial)


Trail_df <- bind_rows(Trial, Anthesis)



# Check it matches --------------------------------------------------------------



names(Trail_df)


unique(Trail_df$Treatment)


# choose only the treatments I modelled ---------------------------------------

Trail_df <- Trail_df %>% filter(
  Treatment == "Control" |
    Treatment == "District_Practice" |
    Treatment == "Nbank_Conservative" |
    Treatment == "Nbank_Optimum_Profit" |
    Treatment == "Nbank_Optimum_Yield" |
    
    Treatment == "YP_Decile1" |
    Treatment == "YP_Decile2-3" |
    Treatment == "YP_Decile5" |
    Treatment == "YP_Decile7-8" 
)








write.csv(Trail_df , 
          "X:/Riskwi$e/Bute/3_Sims_post_Nov2024/results/merge_trial_harvest_and_anthesis.csv", row.names = FALSE )



