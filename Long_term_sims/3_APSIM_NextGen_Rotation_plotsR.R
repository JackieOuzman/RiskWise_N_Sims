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
APSIM_NextGen_daily %>%  distinct(Treatment)
#umm <- distinct(APSIM_NextGen_daily,Phenology)
#APSIM_NextGen_daily$Phenology <- trimws(APSIM_NextGen_daily$Phenology)

str(APSIM_NextGen_daily)
## N at sowing and total N applied

N_applied_in_season <- 
  APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
           Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  filter(FertNApplied > 0)  %>% 
  filter(is.na(Phenology)|Phenology!="Sowing") %>% 
   
  group_by(Treatment, Year) %>% 
  summarise(
  N_applied_in_season = sum(FertNApplied))
N_applied_in_season


# Remove the fallow and create details for sowing and harvest ----


APSIM_NextGen_daily_no_fallow_sowing <- APSIM_NextGen_daily %>% 
  filter(Crop == "Wheat"|
         Crop == "Barley"| 
           Crop == "Chickpea"|
           Crop == "Canola") %>% 
  
  filter(Phenology == "Sowing") %>% 
  select(Date , Year ,Crop, Phenology  , FertNApplied, Soil_NO3, Soil_NH4, Treatment) %>% 
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
         Sowing_Soil_NH4,
         Treatment
         )

sowing_conditions_info <- "25th April – 7th June (Wheat and barley), 25th april - 21st May (canola and chickpea), 
Min ext soil water for sowing = 10, 
Accumulated rainfall required for sowing = 10
Duration of rainfall accumulation = 3
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
                          by = join_by(Year, Treatment))

Year_summary <- left_join(Year_summary, N_applied_in_season,
                          by = join_by(Year, Treatment))

str(Year_summary)

Year_summary <- Year_summary %>% 
  mutate( N_applied_in_season = ifelse(is.na( N_applied_in_season), 0,  
                                       N_applied_in_season))



names(Year_summary)
Year_summary <- Year_summary %>% 
  select(  "Year"  ,
           "Crop",
           "Treatment"  ,
           "Sowing_NApplied", 
           "N_applied_in_season" ,
           "Sowing_Date",
           "Sowing_month" ,
           "Sowing_Soil_NO3" ,
           "Sowing_Soil_NH4"  ,
           "Harvest_Date" ,
           "Harvest_Soil_NO3",
            "Harvest_Soil_NH4",
           "Harvest_month"   ,
           "Harvest_Biomass"   ,
           "Harvest_Yield" ,
           "Harvest_Protien"  ,
           "Harvest_HarvestIndex"
           )

rm(APSIM_NextGen_daily_no_fallow_sowing, APSIM_NextGen_daily_no_fallow_harvest)
str(Year_summary)
distinct(Year_summary, Treatment)

Year_summary_Nil <- Year_summary %>% filter(Treatment == "Nil")
Year_summary_National <- Year_summary %>% filter(Treatment == "National Av")


# TREATMENT - Nil -----
NIL_percentage_sim_with_yld<- 
  count(Year_summary_Nil %>% filter(Harvest_Yield >0 ))/ 
  Year_summary_Nil %>% tally() *100
NIL_number_of_sims <- Year_summary_Nil %>% tally()
NIL_percentage_sim_with_yld
NIL_number_of_sims

## Plots -----

##### Sowing details ----
Nil_Sowing_details <-Year_summary_Nil %>% 
  ggplot(aes(x = Year  , y = Sowing_month , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(plot.subtitle = element_text(size = 8),
        legend.position = "none")+
  labs(title = paste0("Curyo: Nil treatment.",
       " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = sowing_conditions_info,
       caption = paste0(NIL_number_of_sims, " Sims, Years =1960-2024.",
                        " Sims with yld =",NIL_percentage_sim_with_yld,"%" ),
       y = "Month of sowing",
       x = "")
       
Nil_Sowing_details

ggsave(plot = Nil_Sowing_details,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Nil Sowing.png"), width = 20, height = 12, units = "cm")


##### yield details ----
str(Year_summary_Nil)
NIL_quantile_yld <- Year_summary_Nil %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Yield,probs=0.25),
            quantile50=quantile(Harvest_Yield,probs=0.5),
            quantile75=quantile(Harvest_Yield,probs=0.75))

NIL_quantile_yld
Nil_Yield <-Year_summary_Nil %>% 
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
       caption = paste0(NIL_number_of_sims, " Sims, Years =1960-2024.", 
                        " Sims with yld =",
                        NIL_percentage_sim_with_yld,"%" ),
       y = "Yield t/ha",
       x = "")+ 
   geom_hline(data= NIL_quantile_yld, 
              aes(
                yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= NIL_quantile_yld, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= NIL_quantile_yld, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)
  
Nil_Yield
ggsave(plot = Nil_Yield,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Nil Yield.png"), width = 20, height = 12, units = "cm")




##### Protein details ----
str(Year_summary_Nil)
NIL_quantile_Protein <- Year_summary_Nil %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Protien,probs=0.25, na.rm = TRUE),
            quantile50=quantile(Harvest_Protien,probs=0.5, na.rm = TRUE),
            quantile75=quantile(Harvest_Protien,probs=0.75, na.rm = TRUE))

NIL_quantile_Protein

Nil_Protein <-Year_summary_Nil %>% 
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
       caption = paste0(NIL_number_of_sims, " Sims, Years =1960-2024.",
                        " Sims with yld =",NIL_percentage_sim_with_yld,"%" ),
       y = "Protien",
       x = "")+ 
  geom_hline(data= NIL_quantile_Protein, 
             aes(
               yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= NIL_quantile_Protein, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= NIL_quantile_Protein, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)

Nil_Protein


ggsave(plot = Nil_Protein,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo NIL Protein.png"), width = 20, height = 12, units = "cm")



##### Napplied details ----
str(Year_summary_Nil)
Nil_N_applied    <-Year_summary_Nil %>% 
  ggplot(aes(x = Year  , y = Sowing_NApplied , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "N applied at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "FertNApplied",
       x = "Year of simulation")
Nil_N_applied

ggsave(plot = Nil_N_applied,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Nil N_applied.png"), width = 20, height = 12, units = "cm")

##### NO3NO4 in soils ----
Nil_NO3NO4_insoil_sowing    <-Year_summary_Nil %>% 
  ggplot(aes(x = Year  , y =  (Sowing_Soil_NO3 +  Sowing_Soil_NH4),
             group=Crop) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Soil NO3 + NO4 at sowing-  treatment Nil",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Soil NO3 + NO4 at sowing",
       x = "Year of simulation")
Nil_NO3NO4_insoil_sowing

ggsave(plot = Nil_NO3NO4_insoil_sowing,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo Nil NO3NO4_insoil_sowing.png"), width = 20, height = 12, units = "cm")






# TREATMENT - National Av -----
National_percentage_sim_with_yld<- 
  count(Year_summary_National %>% filter(Harvest_Yield >0 ))/ 
  Year_summary_National %>% tally() *100
National_number_of_sims <- Year_summary_National %>% tally()
National_percentage_sim_with_yld
National_number_of_sims

## Plots -----

##### Sowing details ----
National_Sowing_details <-Year_summary_National %>% 
  ggplot(aes(x = Year  , y = Sowing_month , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(plot.subtitle = element_text(size = 8),
        legend.position = "none")+
  labs(title = paste0("Curyo: National treatment.",
                      " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = sowing_conditions_info,
       caption = paste0(National_number_of_sims, " Sims, Years =1960-2024.",
                        " Sims with yld =",National_percentage_sim_with_yld,"%" ),
       y = "Month of sowing",
       x = "")

National_Sowing_details

ggsave(plot = National_Sowing_details,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo National Sowing.png"), width = 20, height = 12, units = "cm")


##### yield details ----
str(Year_summary_National)
National_quantile_yld <- Year_summary_National %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Yield,probs=0.25),
            quantile50=quantile(Harvest_Yield,probs=0.5),
            quantile75=quantile(Harvest_Yield,probs=0.75))

National_quantile_yld
National_Yield <-Year_summary_National %>% 
  ggplot(aes(x = Year  , y = Harvest_Yield       , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(
    #plot.subtitle = element_text(size = 8),
    legend.position = "none")+
  
  labs(title = paste0("Curyo: National treatment.",
                      " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = "Quantiles 0.25, 0.5, 0.75",
       caption = paste0(National_number_of_sims, " Sims, Years =1960-2024.", 
                        " Sims with yld =",
                        National_percentage_sim_with_yld,"%" ),
       y = "Yield t/ha",
       x = "")+ 
  geom_hline(data= National_quantile_yld, 
             aes(
               yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= National_quantile_yld, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= National_quantile_yld, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)

National_Yield
ggsave(plot = National_Yield,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo National Yield.png"), width = 20, height = 12, units = "cm")




##### Protein details ----
str(Year_summary_National)
National_quantile_Protein <- Year_summary_National %>% group_by(Crop ) %>%
  summarize(quantile25=quantile(Harvest_Protien,probs=0.25, na.rm = TRUE),
            quantile50=quantile(Harvest_Protien,probs=0.5, na.rm = TRUE),
            quantile75=quantile(Harvest_Protien,probs=0.75, na.rm = TRUE))

National_quantile_Protein

National_Protein <-Year_summary_National %>% 
  ggplot(aes(x = Year  , y = Harvest_Protien       , group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  theme_bw()+
  theme(
    #plot.subtitle = element_text(size = 8),
    legend.position = "none")+
  
  labs(title = paste0("Curyo: National treatment.",
                      " Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B."),
       subtitle = "Quantiles 0.25, 0.5, 0.75",
       caption = paste0(National_number_of_sims, " Sims, Years =1960-2024.",
                        " Sims with yld =",National_percentage_sim_with_yld,"%" ),
       y = "Protien",
       x = "")+ 
  geom_hline(data= National_quantile_Protein, 
             aes(
               yintercept = quantile25), alpha = 0.4, linetype = 2)+
  geom_hline(data= National_quantile_Protein, 
             aes(yintercept = quantile50), alpha = 0.4)+
  geom_hline(data= National_quantile_Protein, 
             aes(yintercept = quantile75), alpha = 0.4, linetype = 2)

National_Protein


ggsave(plot = National_Protein,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo National Protein.png"), width = 20, height = 12, units = "cm")



##### Napplied details ----
str(Year_summary_National)
Year_summary_National <- Year_summary_National %>% 
  mutate(Total_NApplied = (Sowing_NApplied + N_applied_in_season))

National_N_applied_total    <-Year_summary_National %>% 
  ggplot(aes(x = Year  , y = Total_NApplied, group=Crop        ) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "N applied total -  treatment National",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "FertNApplied",
       x = "Year of simulation")
National_N_applied_total

ggsave(plot = National_N_applied_total,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo National N_applied Total.png"), width = 20, height = 12, units = "cm")






##### NO3NO4 in soils ----
National_NO3NO4_insoil_sowing    <-Year_summary_National %>% 
  ggplot(aes(x = Year  , y =  (Sowing_Soil_NO3 +  Sowing_Soil_NH4),
             group=Crop) )+
  geom_point(aes(color=Crop        ))+
  facet_wrap(.~Crop)+
  labs(title = "Soil NO3 + NO4 at sowing-  treatment National",
       subtitle = "Rotaion sequence W-> C->W->B->W->Lentil(Chickpea)-> B.",
       caption = "Years 1960-2024",
       y = "Soil NO3 + NO4 at sowing",
       x = "Year of simulation")
National_NO3NO4_insoil_sowing

ggsave(plot = National_NO3NO4_insoil_sowing,
       filename =  paste0("X:/Riskwi$e/Curyo/4_Long term sims/Results/",
                          "Curyo National NO3NO4_insoil_sowing.png"), width = 20, height = 12, units = "cm")









write.csv(Year_summary ,
          "X:/Riskwi$e/Curyo/4_Long term sims/Results/Year_summary_all.csv", 
          row.names = FALSE )
write.csv(Year_summary_Nil ,
          "X:/Riskwi$e/Curyo/4_Long term sims/Results/Year_summary_Nil.csv", 
          row.names = FALSE )
write.csv(Year_summary_National ,
          "X:/Riskwi$e/Curyo/4_Long term sims/Results/Year_summary_National.csv", row.names = FALSE )
