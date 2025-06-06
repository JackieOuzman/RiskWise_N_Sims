# This file is for plotting frost days for one year and grain yld and flowering date
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
                                              #X:\Riskwi$e\Dry_sowing\Kyancutta\Results

Dry_sowing_Kyancutta_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Kyancutta/Results/WB_10/WB10_Dry_sowing_Kyancutta_factor_with_met_18170_v2_gs.csv")
str(Dry_sowing_Kyancutta_factor_with_met)

unique(Dry_sowing_Kyancutta_factor_with_met$Wheat.Phenology.CurrentStageName)
unique(Dry_sowing_Kyancutta_factor_with_met$Sowing_date)
unique(Dry_sowing_Kyancutta_factor_with_met$year)

         ## Filter just to plot one year and one sowing date - frost year with typical sowing date

Sim_met_may102023 <- Dry_sowing_Kyancutta_factor_with_met %>% 
  #filter(year == 2023) %>% 
  filter(Sowing_date == "10-may")
Sim_met_may102023
unique(Sim_met_may102023$year)


unique(Sim_met_may102023$Wheat.Phenology.CurrentStageName)
phenology_stages <- Sim_met_may102023 %>% filter(!is.na(Wheat.Phenology.CurrentStageName)) %>% 
  select(Clock.Today,Wheat.Phenology.CurrentStageName )
phenology_stages

str(Sim_met_may102023)
unique(Sim_met_may102023$frost_Sensitive_period)
Frost_senstive_period <- Sim_met_may102023 %>% 
  select(Clock.Today, frost_Sensitive_period ) %>% 
  filter(frost_Sensitive_period == "frost_sensitive_period" )
Frost_senstive_period

Frost_most_senstive_period <- Sim_met_may102023 %>% 
  select(Clock.Today, frost_most_Sensitive_period ) %>% 
  filter(frost_most_Sensitive_period == "frost_most_sensitive_period" )
Frost_most_senstive_period

phenology_stages

Anthesis_Flowering <- phenology_stages %>% filter(Wheat.Phenology.CurrentStageName== "Anthesis" )


plot1 <- Sim_met_may102023 %>% 
  ggplot(aes(x = Clock.Today, mint))+
 
  geom_vline(data = Frost_senstive_period, aes(xintercept = Clock.Today), color = "lightgrey", linewidth = 2)+
  geom_vline(data = Anthesis_Flowering, aes(xintercept = Clock.Today), color = "darkgreen", linewidth = 2)+
 
  geom_point()+
  
  theme_classic()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,  linetype="dashed", color = "red")+
  annotate("text", x= as.Date("2023-10-30"), y=1.5, label="Frost definition", color = "red")+ 
  geom_vline(data = phenology_stages, aes(xintercept = Clock.Today), linetype="dashed", color = "grey")+
  
  labs(title = "Climate station Kyancutta 18046",
       subtitle = "Days from sowing to harvest only. Dry sowing dates 10-May 2023.\nOptimal germination conditions using water balance with threshold of 10mm",
       x = "",
       y = "Min temp",
       caption = "Dashed grey lines indicate phenology stages. \n Green line is flowering. \nSolid grey is frost senstive period (Phenology.Stage between 6.49 - 9.5)")
plot1


path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Kyancutta", "Results", "WB_10")

# ggsave(plot = plot1,
#        filename = paste0(path_saved_files,"/MinTemp_vs_days_growthStages_Kyancutta2023_v2", ".png" ),
#        width = 20, height = 12, units = "cm")
# 

#### grain yld vs flowering date



str(Dry_sowing_Kyancutta_factor_with_met)
flowering_date <- Dry_sowing_Kyancutta_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Anthesis"  ) %>% 
  select(year, Clock.Today, Sowing_date) 

Yield <- Dry_sowing_Kyancutta_factor_with_met %>% 
 group_by(year, Sowing_date) %>% 
  summarise(max_yld_t_ha = max(Yield_Adj_t_ha,na.rm = TRUE)) 


Yld_flowering <- left_join(flowering_date, Yield)
Yld_flowering <- Yld_flowering %>% mutate(Julian_days =Clock.Today )

Yld_flowering$Julian_days <- lubridate::yday(as.Date(Yld_flowering$Julian_days))

distinct(Yld_flowering,Sowing_date )


Yld_flowering$Sowing_date <- factor(Yld_flowering$Sowing_date , ordered = TRUE, 
                                            levels = c(
                                              "1-apr" ,
                                              "5-apr" ,
                                              "10-apr" ,
                                              "15-apr" ,
                                              "20-apr",
                                              "25-apr",
                                              "30-apr",
                                              
                                              "1-may" ,
                                              "5-may" ,
                                              "10-may" ,
                                              "15-may" ,
                                              "20-may",
                                              "25-may",
                                              "30-may",
                                             
                                               "1-jun" ,
                                              "5-jun" ,
                                              "10-jun" ,
                                              "15-jun" ,
                                              "20-jun",
                                              "25-jun"
                                            ))

Yld_flowering <- Yld_flowering %>% 
  mutate(optimal_flowering_start = paste0(year(Clock.Today), "-09-04"),
         optimal_flowering_end = paste0(year(Clock.Today), "-09-14")) 

Yld_flowering <- Yld_flowering %>% 
  mutate(Julian_optimal_flowering_start = lubridate::yday(as.Date(optimal_flowering_start)),
         Julian_optimal_flowering_end = lubridate::yday(as.Date(optimal_flowering_end)) )                                                       
names(Yld_flowering)


str(Yld_flowering)

#these steps are stuffing around plotting month an axis rather than DOY/Julian days
Yld_flowering$CommonDate <- ymd(paste0("2000-",str_sub(as.character(Yld_flowering$Clock.Today),-5)))
Yld_flowering$CommonDateOptimalFlowerStart <- ymd(paste0("2000-","-09-04"))
Yld_flowering$CommonDateOptimalFlowerEnd <- ymd(paste0("2000-","-09-14"))  
  
 

plot2 <- Yld_flowering %>% 
  #ggplot(aes(x =lubridate::yday(Clock.Today), max_yld_t_ha))+
  ggplot(aes(x =CommonDate, max_yld_t_ha))+
  
   annotate("rect", 
             xmin = min(Yld_flowering$CommonDateOptimalFlowerStart),
             xmax = min(Yld_flowering$CommonDateOptimalFlowerEnd), 
             ymin = 0, ymax = 5,
            alpha = .2,  fill = "darkgreen")+

  geom_point()+
   geom_point(data= Yld_flowering %>% filter(year == 2023), 
                aes(x =CommonDate, max_yld_t_ha), colour ="red")+
  facet_wrap(.~ Sowing_date)+
  
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Yield vs flowering dates Kyancutta 18046",
       subtitle = "Dry sowing dates as facet. 
       Optimal germination conditions using water balance with threshold of 10mm
       If optimal germination conditions are not met no crop will reach flowering",
       x = "Flowering dates",
       y = "Yield t/ha",
       caption = "Green line indicated optimal flowering dates. Red dot =2023")
plot2

ggsave(plot = plot2,
       filename = paste0(path_saved_files,"/WB10_FloweringJulianDays_vs_yiled_Kyancutta_v2", ".png" ),
       width = 20, height = 12, units = "cm")

str(Yld_flowering)

#### grain yld vs dry sowing date

Yld_flowering

plot2_box <- Yld_flowering %>% 
  ggplot(aes(x =Sowing_date, max_yld_t_ha))+
  geom_boxplot()+
  geom_point(alpha =0.2)+
  geom_point(data= Yld_flowering %>% filter(year == 2023), 
             aes(x =Sowing_date, max_yld_t_ha), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  
  ylim(0,6)+
  labs(title = "Yield vs dry sowing dates Kyancutta 18046.\nOptimal germination conditions using water balance with threshold of 10mm",
       subtitle = "This is not the optimal germination conditions, but the start of window.\nIf optimal germination conditions are not met no gernimation will occur and no yield will be produced.",
       y = "Yield t/ha",
       x ="Dry sowing date",
       caption = "Red dot = 2023.\nMany dry sowing dates recorded:\nLow yields in 1977.\nHigh yields in 1996"
       )
plot2_box




check_1_april <- Yld_flowering %>% filter(Sowing_date == "1-apr") %>% select(year, Clock.Today, max_yld_t_ha)
check <- Yld_flowering %>% select(year, Clock.Today, max_yld_t_ha, Sowing_date)

ggsave(plot = plot2_box,
       filename = paste0(path_saved_files,"/WB10_Box_yield_vs_drySowing_datesKyancutta_v2", ".png" ),
       width = 20, height = 12, units = "cm")





#### grain yld vs germination date
str(Dry_sowing_Kyancutta_factor_with_met)
#get the germination dates 
distinct(Dry_sowing_Kyancutta_factor_with_met,Wheat.Phenology.CurrentStageName )
germination_dates <- Dry_sowing_Kyancutta_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Germination") %>% 
  select(year, Sowing_date, Clock.Today,Wheat.Phenology.CurrentStageName)
germination_dates

## get the yield from the flowering df
str(Yld_flowering)
Yld <- Yld_flowering %>% select(year, Sowing_date, max_yld_t_ha)

## make new df with germination dates and yld
Yld_germination_date <- left_join(Yld, germination_dates)

#these steps are stuffing around plotting month an axis rather than DOY/Julian days
Yld_germination_date$CommonDate <- ymd(paste0("2000-",str_sub(as.character(Yld_germination_date$Clock.Today),-5)))


# make a clm which groups germination dates

Yld_germination_date <- Yld_germination_date %>% 
  mutate(germination_between = case_when(
    between(CommonDate, as.Date("2000-04-01"), as.Date("2000-04-05"))     ~ "1 to 5 Apr",
    between(CommonDate, as.Date("2000-04-05"), as.Date("2000-04-10"))     ~ "5 to 10 Apr",
    between(CommonDate, as.Date("2000-04-10"), as.Date("2000-04-15"))     ~ "10 to 15 Apr",
    between(CommonDate, as.Date("2000-04-15"), as.Date("2000-04-20"))     ~ "15 to 20 Apr",
    between(CommonDate, as.Date("2000-04-20"), as.Date("2000-04-25"))     ~ "20 to 25 Apr",
    between(CommonDate, as.Date("2000-04-25"), as.Date("2000-04-30"))     ~ "25 to 30 Apr",
    
    between(CommonDate, as.Date("2000-05-01"), as.Date("2000-05-05"))     ~ "1 to 5 May",
    between(CommonDate, as.Date("2000-05-05"), as.Date("2000-05-10"))     ~ "5 to 10 May",
    between(CommonDate, as.Date("2000-05-10"), as.Date("2000-05-15"))     ~ "10 to 15 May",
    between(CommonDate, as.Date("2000-05-15"), as.Date("2000-05-20"))     ~ "15 to 20 May",
    between(CommonDate, as.Date("2000-05-20"), as.Date("2000-05-25"))     ~ "20 to 25 May",
    between(CommonDate, as.Date("2000-05-25"), as.Date("2000-05-31"))     ~ "25 to 31 May",
    
    between(CommonDate, as.Date("2000-06-01"), as.Date("2000-06-05"))     ~ "1 to 5 Jun",
    between(CommonDate, as.Date("2000-06-05"), as.Date("2000-06-10"))     ~ "5 to 10 Jun",
    between(CommonDate, as.Date("2000-06-10"), as.Date("2000-06-15"))     ~ "10 to 15 Jun",
    between(CommonDate, as.Date("2000-06-15"), as.Date("2000-06-20"))     ~ "15 to 20 Jun",
    between(CommonDate, as.Date("2000-06-20"), as.Date("2000-06-25"))     ~ "20 to 25 Jun",
    between(CommonDate, as.Date("2000-06-25"), as.Date("2000-06-30"))     ~ "25 to 30 Jun",
    
    between(CommonDate, as.Date("2000-06-30"), as.Date("2000-07-30"))     ~ "Jul",
    TRUE  ~ "Unknown"
    
  ))

Yld_germination_date$germination_between <- factor(Yld_germination_date$germination_between , ordered = TRUE, 
                                    levels = c(
                                      "1 to 5 Apr",
                                      "5 to 10 Apr",
                                      "10 to 15 Apr",
                                      "15 to 20 Apr",
                                      "20 to 25 Apr",
                                      "25 to 30 Apr",
                                      
                                      
                                      "1 to 5 May",
                                      "5 to 10 May",
                                      "10 to 15 May",
                                      "15 to 20 May",
                                      "20 to 25 May",
                                      "25 to 31 May",
                                      
                                      "1 to 5 Jun",
                                      "5 to 10 Jun",
                                      "10 to 15 Jun",
                                      "15 to 20 Jun",
                                      "20 to 25 Jun",
                                      "25 to 30 Jun",
                                      "Jul"
                                    ))







plot3_box <- Yld_germination_date %>% 
  ggplot(aes(x =germination_between, max_yld_t_ha))+
  geom_boxplot()+
  geom_point(alpha =0.2)+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  
  ylim(0,6)+
  labs(title = "Yield vs germination dates Kyancutta 18046. \nOptimal germination conditions using water balance with threshold of 10mm",
       subtitle = "Years when germination date falls inside date window on x axis\nIf optimal germination conditions are not met no gernimation will occur and no yield will be produced.",
       y = "Yield t/ha",
       x ="Date range when germination occured"
       
  )
plot3_box

ggsave(plot = plot3_box,
       filename = paste0(path_saved_files,"/WB10_Box_yield_vs_germination_datesKyancutta_v2", ".png" ),
       width = 20, height = 12, units = "cm")




#################################################################################
#More details for plot 3 Yld vs germination dates.
# how many data point per grouping
# colour the sowing dates

str(Yld_germination_date)

count_yrs_germination_date <- Yld_germination_date %>% 
  group_by(germination_between) %>% 
  summarise(count_years = n())


Yld_germination_date$Sowing_date <- factor(
  Yld_germination_date$Sowing_date ,
  ordered = TRUE,
  levels = c(
    "1-apr" ,
    "5-apr" ,
    "10-apr" ,
    "15-apr" ,
    "20-apr",
    "25-apr",
    "30-apr",
    
    "1-may" ,
    "5-may" ,
    "10-may" ,
    "15-may" ,
    "20-may",
    "25-may",
    "30-may",
    
    "1-jun" ,
    "5-jun" ,
    "10-jun" ,
    "15-jun" ,
    "20-jun",
    "25-jun"
  )
)


plot4_box <- Yld_germination_date %>% 
  ggplot(aes(x =germination_between, max_yld_t_ha))+
  geom_boxplot()+
  geom_text(data = count_yrs_germination_date, aes(x = germination_between, y = 5, 
                                                   label = count_years))+
 
  geom_point(data = Yld_germination_date, aes(x =germination_between, max_yld_t_ha, 
                                              colour = as.factor(Sowing_date)),alpha =0.2)+
  theme_classic()+
  theme(
        legend.position = "bottom",
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  
  ylim(0,6)+
  labs(title = "Yield vs germination dates Kyancutta 18046. \nOptimal germination conditions using water balance with threshold of 10mm",
       subtitle = "Years when germination date falls inside date window on x axis\nIf optimal germination conditions are not met no gernimation will occur and no yield will be produced.",
       y = "Yield t/ha",
       x ="Date range when germination occured"
       
  )
plot4_box

ggsave(plot = plot4_box,
       filename = paste0(path_saved_files,"/WB10_Box_yield_vs_germination_dates_count_coloursKyancutta_v2", ".png" ),
       width = 20, height = 12, units = "cm")
