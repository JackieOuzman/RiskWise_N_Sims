library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

Trial_setup_outputs <- read_excel("X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_database_only.xlsx", 
                                  sheet = "Annual plot data reformatted 25", skip = 1)
names(Trial_setup_outputs)

df_selection  <- Trial_setup_outputs %>% 
  select(Year , 
         Crop = `Species` ,
         Treatment = `Treatment name`, #was System 
         InCropFert =  `Fertiliser total N rate (kg/ha)` ,
         Yield_t_ha = `Yield (t/ha)`,
         Harvest_Date_M = `Harvest date (machine)`,
         Grain_moisture = `Grain moisture (%)`
         
  )
df_selection
unique(df_selection$Treatment)

df_selection <- df_selection %>% 
  mutate(CurrentState = case_when(
    Year == 2018  ~ "Wheat1",
    Year == 2019  ~ "canola",
    Year == 2020  ~ "Wheat2",
    Year == 2021  ~ "Barley_1",
    Year == 2022  ~ "Wheat3",
    Year == 2023  ~ "chickpea",
    Year == 2024  ~ "Barley_2" ))

df_selection <- df_selection %>% 
  mutate(Zone = case_when(
    Treatment == "01_Nil"  ~ "Nil",
    Treatment == "02_Replacement"  ~ "Replacment",
    Treatment == "03_National_Average"  ~ "National_Av",
    Treatment == "04_100kgMaint"  ~ "Maint_100",
    Treatment == "05_125kgMaint"  ~ "Maint_125",
    Treatment == "06_150kgMaint"  ~ "Maint_150",
    Treatment == "07_100%YP"  ~ "YP_100",
    Treatment == "08_75%YP"  ~ "YP_75",
    Treatment == "09_50%YP"  ~ "YP_50",
    Treatment == "10_25%YP"  ~ "YP_25"
    ))



crops_yld <- read_csv( "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/crops_yld_trial.csv")

names(df_selection)
names(crops_yld)
unique(crops_yld$Zone)

Curyo_Sims_obs <- left_join(df_selection, crops_yld, join_by("CurrentState", "Zone"))
names(Curyo_Sims_obs)



Curyo_Sims_obs <- Curyo_Sims_obs %>% select(
  Year ,
  Crop  ,
  Treatment  ,
  InCropFert  ,
  Yield_t_ha  ,
  Harvest_Date_M ,
   CurrentState, 
   Zone ,
  Yieldkgha
) %>% 
  mutate(Yield_t_ha_observed =Yield_t_ha,
         Treatment_name_short =Zone ,
          Yield_t_ha_predicted = Yieldkgha /1000) %>% 
  select(-Yield_t_ha, - Zone, -Yieldkgha)



write_csv(Curyo_Sims_obs, "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_Sims_obs.csv")

long_Curyo_Sims_obs <- Curyo_Sims_obs %>% pivot_longer(
  cols = starts_with("Yield_t_ha_"),
  names_to = "obs_predicted",
  #names_prefix = "Yield",
  values_to = "Yield",
  values_drop_na = TRUE
)

N_Response_plot <- long_Curyo_Sims_obs %>% 
  ggplot(mapping = aes(x=InCropFert, y = Yield, colour = obs_predicted) )+
  geom_point(size = 2)+
  scale_color_manual(values = c("blue", "black")) +
  theme_bw() +
  labs(
    title = "N response to applied fertiliser",
    colour = "",
    x = "InCropFert",
    y = ""  )+
  facet_wrap(.~Year)


N_Response_plot
