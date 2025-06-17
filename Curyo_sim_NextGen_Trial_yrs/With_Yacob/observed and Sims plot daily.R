#Next_gen_plots

library(data.table)

# Daily output files -------------------------------------------------------

APSIM_NextGen_daily <- read_csv("X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_7_Rotation_N_bank std output Trial.DailyReport.csv")
crops_obs_pred <- read_csv( "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Curyo_Sims_obs.csv")

str(APSIM_NextGen_daily)
APSIM_NextGen_daily <- APSIM_NextGen_daily %>%   mutate(year = year(Date), 
    Crop = case_when(
      year  == 2018 & CurrentState  == "Wheat1"~ "Wheat",
      year  == 2019 & CurrentState  == "Canola1"~ "Canola",
      year  == 2020 & CurrentState  == "Wheat2"~ "Wheat",
      year  == 2021 & CurrentState  == "Barley_1"~ "Barley",
      year  == 2022 & CurrentState  == "Wheat3"~ "Wheat",
      year  == 2023 & CurrentState  == "Chickpea"~ "Chickpea",
      year  == 2024 & CurrentState  == "Barley_2"~ "Barley"  ),
    
    Yield = case_when(
      year  == 2018 ~ WheatYieldkgha/1000,
      year  == 2019 ~ CanolaYieldkgha/1000,
      year  == 2020 ~ WheatYieldkgha/1000,
      year  == 2021 ~ BarleyYieldkgha/1000,
      year  == 2022 ~ WheatYieldkgha/1000,
      year  == 2023 ~ ChickpeaYieldkgha/1000,
      year  == 2024 ~ BarleyYieldkgha/1000
    ))
    
APSIM_NextGen_daily_select <- APSIM_NextGen_daily %>% 
  select(year, Zone, Date , FertNApplied, Crop, Yield, CurrentState  )    %>% 
  filter( !is.na(Crop)) %>% 
  rename(Yield_predicted = Yield)


###############################################################################
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

### I cant access this dirctory now??

str(crops_obs_pred)
names(crops_obs_pred)
trial_data_only <- crops_obs_pred %>% select("Year",
                                             "Crop",
                                             "Treatment",
                                             "InCropFert",
                                             "Harvest_Date_M"  ,
                                             "Treatment_name_short",
                                             "Yield_t_ha_observed") %>% 
rename(Zone = Treatment_name_short) %>% 
  mutate(zone_year =paste0(Zone,"_",Year ))

trial_data_only<- trial_data_only %>% 
  group_by(zone_year) %>% 
  mutate(rep = as.integer(gl(n(), 1, n()))) %>%
  ungroup %>% 
  arrange(Treatment, Year) %>% 
  mutate(Date = as.Date(Harvest_Date_M))
### add this to my sims for the day of harvest
str(trial_data_only$Date)
str(APSIM_NextGen_daily_select$Date)

unique(APSIM_NextGen_daily$Zone)
unique(trial_data_only$Treatment)



### Help with plotting
APSIM_NextGen_daily_select$Zone <- factor(APSIM_NextGen_daily_select$Zone , ordered = TRUE, 
                                       levels = c(
                                         "Nil" ,
                                         "Replacment" ,
                                         "National_Av" ,
                                         "Maint_100" ,
                                         "Maint_125",
                                         "Maint_150",
                                         "YP_100",
                                         "YP_75" ,
                                         "YP_50",
                                         "YP_25" 
                                       ))

trial_data_only$Zone <- factor(trial_data_only$Zone , ordered = TRUE, 
                                          levels = c(
                                            "Nil" ,
                                            "Replacment" ,
                                            "National_Av" ,
                                            "Maint_100" ,
                                            "Maint_125",
                                            "Maint_150",
                                            "YP_100",
                                            "YP_75" ,
                                            "YP_50" ,
                                            "YP_25"
                                          ))


plotyld <- APSIM_NextGen_daily_select %>%
  filter(!is.na(Zone)) %>% 
  ggplot(aes(x = (Date), y = Yield_predicted )) +
  #geom_point(size = 1, colour = "blue") +
  geom_line(colour = "blue")+
  theme_bw() +
  labs(
    title = "Yield Cuyro Sims ",
    caption =   "2023 has no trial harvest date.\nTrial data is machine harvest area and moisture corrected.\nSimulation data not adjusted for moisture",
    colour = "",
    x = "Year",
    y = ""   
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Zone)+
  
  trial_data_only %>%
  filter(!is.na(Zone)) %>%  
  geom_point(mapping = aes(x = Date, y = Yield_t_ha_observed),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plotyld


###############################################################################
## adjust sim for moisture
names(APSIM_NextGen_daily_select)
unique(APSIM_NextGen_daily_select$Crop)

APSIM_NextGen_daily_select <- APSIM_NextGen_daily_select %>% 
  mutate(demoniator = 
           case_when(
             Crop == "Chickpea" ~ 0.860, # 1-(14/100)
             Crop == "Wheat" ~   0.875,  # 1-(12.5/100)
             Crop == "Canola" ~  0.875,  # 1-(12.5/100)
             Crop == "Barley" ~  0.875,  # 1-(12.5/100)
             ),
         numerator = (1-(Yield_predicted/100)),
         Yield_predicted_Moisture_adjused = Yield_predicted*(numerator/demoniator)
         ) 


plotyld_moisture <- APSIM_NextGen_daily_select %>%
  filter(!is.na(Zone)) %>% 
  ggplot(aes(x = (Date), y = Yield_predicted_Moisture_adjused )) +
  geom_line(colour = "darkgreen")+
  geom_line(aes(x = Date, y = Yield_predicted ), colour = "blue")+
  theme_bw() +
  labs(
    title = "Yield Cuyro Sims ",
    caption =   "2023 has no trial harvest date.\nTrial data is machine harvest area and moisture corrected.\nSimulation data adjusted for moisture 12.5 and 14%.\nPredYld*((1-(PredYld/100))/(1-(12.5/100)))",
    colour = "",
    x = "Year",
    y = ""   
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_wrap(.~Zone)+
  
  trial_data_only %>%
  filter(!is.na(Zone)) %>%  
  geom_point(mapping = aes(x = Date, y = Yield_t_ha_observed),
             colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plotyld_moisture

path <- "X:/Riskwi$e/Curyo/3_Sims_post_Nov2024/Results/With_Yacob_July2025/"
ggsave(plot = plotyld_moisture,filename =  paste0(path,"_plotyld_moisture.png"), width = 20, height = 12, units = "cm")
ggsave(plot = plotyld,filename =  paste0(path,"_plotyld.png"), width = 20, height = 12, units = "cm")
