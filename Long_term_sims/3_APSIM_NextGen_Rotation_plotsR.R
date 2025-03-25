# This file is for graph some long term sims Next Gen APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

# Read file ----
APSIM_NextGen_daily <-  read_csv("X:/Riskwi$e/Curyo/4_Long term sims/Results/APSIM_NextGen_Daily.csv" )


APSIM_NextGen_daily %>%  distinct(Crop)
APSIM_NextGen_daily %>%  distinct(Crop)
distinct(APSIM_NextGen_daily,Phenology)

# Remove the fallow and create details for sowing and harvest ----


APSIM_NextGen_daily_no_fallow_sowing <- APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
         Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  
  filter(Phenology == "Sowing") %>% 
  select(Date , Year ,Crop, Phenology  , FertNApplied, Soil_NO3, Soil_NH4) %>% 
  mutate(month = lubridate::month(Date, label = TRUE))
str(APSIM_NextGen_daily_no_fallow_sowing)


APSIM_NextGen_daily_no_fallow_sowing <- APSIM_NextGen_daily_no_fallow_sowing %>% 
  rename(Sowing_Date = Date,
         Sowing_Soil_NO3 = Soil_NO3,
         Sowing_Soil_NH4 = Soil_NH4,
         Sowing_month = month,
         Sowing_NApplied = FertNApplied
         
         )

APSIM_NextGen_daily_no_fallow_sowing <- APSIM_NextGen_daily_no_fallow_sowing %>% 
  select(Year , Crop ,
         Sowing_Date,
         Sowing_month,
         Sowing_NApplied,
         Sowing_Soil_NO3,
         Sowing_Soil_NH4
         )

sowing_conditions_info <- "25 April – 14 April, 
Min ext soil water for sowing = 10, 
Accumulated rainfall required for sowing = 10
Duration of rainfall accumulation = 5
Must sow = yes"

APSIM_NextGen_daily_no_fallow_harvest <- APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
           Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  
  filter(Phenology == "HarvestRipe") %>% 
  mutate(month = lubridate::month(Date, label = TRUE))
str(APSIM_NextGen_daily_no_fallow_harvest)


APSIM_NextGen_daily_no_fallow_harvest <- APSIM_NextGen_daily_no_fallow_harvest %>% 
  rename(Harvest_Date = Date,
         Harvest_Soil_NO3 = Soil_NO3,
         Harvest_Soil_NH4 = Soil_NH4,
         Harvest_month = month,
         Harvest_Biomass = Biomass,
         Harvest_Yield = Yield,
         Harvest_Protien =  Protien ,
         Harvest_HarvestIndex = HarvestIndex
  )

APSIM_NextGen_daily_no_fallow_harvest <- APSIM_NextGen_daily_no_fallow_harvest %>% 
  select(Year , 
         Harvest_Date,
         Harvest_Soil_NO3,
         Harvest_Soil_NH4 ,
         Harvest_month,
         Harvest_Biomass,
         Harvest_Yield ,
         Harvest_Protien  ,
         Harvest_HarvestIndex,
         Treatment   
  )

# Combined summary dataset for year ----
Year_summary <- left_join(APSIM_NextGen_daily_no_fallow_sowing, 
                          APSIM_NextGen_daily_no_fallow_harvest,
                          by = join_by(Year))

rm(APSIM_NextGen_daily_no_fallow_sowing, APSIM_NextGen_daily_no_fallow_harvest)
str(Year_summary)
percentage_sim_with_yld<- count(Year_summary %>% filter(Harvest_Yield >0 ))/ Year_summary %>% tally() *100
number_of_sims <- Year_summary %>% tally()
percentage_sim_with_yld
number_of_sims

# Plots -----

##### Sowing details ----
plot1 <-Year_summary %>% 
  ggplot(aes(x = Year  , y = Sowing_month , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(plot.subtitle = element_text(size = 8),
        legend.position = "none")+
  labs(title = paste0("Curyo: Nil treatment.",
       " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = sowing_conditions_info,
       caption = paste0(number_of_sims, " Sims, Years =1960-2024.", " Sims with yld =",percentage_sim_with_yld,"%" ),
       y = "Month of sowing",
       x = "")
       
plot1

ggsave(plot = plot1,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Sowing.png"), width = 20, height = 12, units = "cm")


##### yield details ----
str(Year_summary)
quantile_yld <- Year_summary %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Yield,probs=0.25),
            quantile50=quantile(Harvest_Yield,probs=0.5),
            quantile75=quantile(Harvest_Yield,probs=0.75))

quantile_yld
Yield <-Year_summary %>% 
  ggplot(aes(x = Year  , y = Harvest_Yield       , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(
        #plot.subtitle = element_text(size = 8),
        legend.position = "none")+
  
  labs(title = paste0("Curyo: Nil treatment.",
                      " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = "Quantiles 0.25, 0.5, 0.75",
       caption = paste0(number_of_sims, " Sims, Years =1960-2024.", " Sims with yld =",percentage_sim_with_yld,"%" ),
       y = "Yield t/ha",
       x = "")+ 
   geom_hline(data= quantile_yld, 
              aes(
                yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= quantile_yld, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= quantile_yld, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)
  
Yield
ggsave(plot = Yield,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Yield.png"), width = 20, height = 12, units = "cm")




##### Protein details ----
str(Year_summary)
quantile_Protein <- Year_summary %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Protien,probs=0.25, na.rm = TRUE),
            quantile50=quantile(Harvest_Protien,probs=0.5, na.rm = TRUE),
            quantile75=quantile(Harvest_Protien,probs=0.75, na.rm = TRUE))

quantile_Protein

Protein <-Year_summary %>% 
  ggplot(aes(x = Year  , y = Harvest_Protien       , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(
    #plot.subtitle = element_text(size = 8),
    legend.position = "none")+
  
  labs(title = paste0("Curyo: Nil treatment.",
                      " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = "Quantiles 0.25, 0.5, 0.75",
       caption = paste0(number_of_sims, " Sims, Years =1960-2024.", " Sims with yld =",percentage_sim_with_yld,"%" ),
       y = "Protien",
       x = "")+ 
  geom_hline(data= quantile_Protein, 
             aes(
               yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= quantile_Protein, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= quantile_Protein, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)

Protein


ggsave(plot = Protein,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Protein.png"), width = 20, height = 12, units = "cm")



##### Napplied details ----
str(Year_summary)
N_applied    <-Year_summary %>% 
  ggplot(aes(x = Year  , y = Sowing_NApplied, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "N applied at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "FertNApplied",
       x = "Year of simulation")
N_applied

ggsave(plot = N_applied,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo N_applied.png"), width = 20, height = 12, units = "cm")

##### NO3NO4 in soils ----
NO3NO4_insoil_sowing    <-Year_summary %>% 
  ggplot(aes(x = Year  , y =  (Sowing_Soil_NO3 +  Sowing_Soil_NH4),
             group=Crop) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Soil NO3 + NO4 at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Soil NO3 + NO4 at sowing",
       x = "Year of simulation")
NO3NO4_insoil_sowing

ggsave(plot = NO3NO4_insoil_sowing,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo NO3NO4_insoil_sowing.png"), width = 20, height = 12, units = "cm")



write.csv(Year_summary ,
          "X:/Riskwi$e/Curyo/4_Long term sims/Results/Year_summary_nil.csv", row.names = FALSE )
