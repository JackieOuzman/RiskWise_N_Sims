# This file is for plotting frost days for one year and grain yld and flowering date
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
 
Dry_sowing_Kyancutta_factor_with_met <- read_csv("X:/Riskwi$e/Dry_sowing/Kyancutta/Results/WB_5/WB_5mmDry_sowing_Kyancutta_factor_with_met_18170_v2_gs.csv")
str(Dry_sowing_Kyancutta_factor_with_met)

unique(Dry_sowing_Kyancutta_factor_with_met$Wheat.Phenology.CurrentStageName)
unique(Dry_sowing_Kyancutta_factor_with_met$Sowing_date)

min(Dry_sowing_Kyancutta_factor_with_met$Clock.Today)
max(Dry_sowing_Kyancutta_factor_with_met$Clock.Today)
number_potentail_years <- max(Dry_sowing_Kyancutta_factor_with_met$year ) - 
                          min(Dry_sowing_Kyancutta_factor_with_met$year )
################################################################################
### not every years triggered a sowing event
Dry_sowing_Kyancutta_factor_with_met %>% distinct(Wheat.Phenology.CurrentStageName)

sowing_germination_event_trigger <- Dry_sowing_Kyancutta_factor_with_met %>% 
  filter(Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  group_by(Sowing_date) %>% 
  summarise(count_yrs_sowing = n()) %>% 
  mutate(number_potentail_years = number_potentail_years)
################################################################################
#Plot years of germination

sowing_germination_event_trigger$Sowing_date <- factor(sowing_germination_event_trigger$Sowing_date , ordered = TRUE, 
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


years_germ <- sowing_germination_event_trigger %>% 
  ggplot(aes(x =Sowing_date, count_yrs_sowing))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept=67)+
  theme(legend.position = "none")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(title = "Count of years when Optimal germination conditions was triggered. ",
       subtitle = "Kyancutta 18170. \nUsing water balance with threshold of 5mm",
       x = "Dry sowing date ie start of window",
       y = "count of years that water balance triggered germination",
       caption = "Years 1957 -2024")
years_germ
################################################################################
path_saved_files <- file_path_input_data<-file.path("X:","Riskwi$e", "Dry_sowing", "Kyancutta", "Results", "WB_5")
ggsave(plot = years_germ,
       filename = paste0(path_saved_files,"/years_Optimal germination conditions was triggered Kyancutta", ".png" ),
       width = 20, height = 12, units = "cm")
################################################################################
#### grain yld 
str(Dry_sowing_Kyancutta_factor_with_met)
Yield <- Dry_sowing_Kyancutta_factor_with_met %>% 
 group_by(year, Sowing_date) %>% 
  summarise(max_yld_t_ha = max(Yield_Adj_t_ha,na.rm = TRUE)) 
Yield <- ungroup(Yield)
distinct(Yield,Sowing_date )

################################################################################
#Dummy years when optimal conditions were not triggered
str(Dry_sowing_Kyancutta_factor_with_met)

start_dates <- Dry_sowing_Kyancutta_factor_with_met %>% distinct(Sowing_date                     )
start_dates_1 <-dplyr::pull(start_dates, Sowing_date)

max(Dry_sowing_Kyancutta_factor_with_met$year)
min(Dry_sowing_Kyancutta_factor_with_met$year)

#days <- seq.Date(as.Date("1957-01-01"),as.Date("2024-12-31"), "days")
years <- seq(1957,2024)
# Create the data frame with all combinations
template <- expand.grid(start_dates_1, years)

# Rename the columns for clarity
colnames(template) <- c("Sowing_date", "year")

str(template$year)
str(template$Sowing_date)
str(Yield)
str(template)

template$year <- as.numeric(template$year)
template$Sowing_date <- as.character(template$Sowing_date)


Yield_with_zeros <- left_join(template, Yield)
Yield_with_zeros <- Yield_with_zeros %>% mutate( max_yld_t_ha = ifelse(is.na( max_yld_t_ha), 0,  max_yld_t_ha))


### Help with plotting
Yield_with_zeros$Sowing_date <- factor(Yield_with_zeros$Sowing_date , ordered = TRUE, 
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


plot2_box <- Yield_with_zeros %>% 
  ggplot(aes(x =Sowing_date, max_yld_t_ha))+
  geom_boxplot()+
  geom_point(alpha =0.2)+
  geom_point(data= Yield %>% filter(year == 2023), 
             aes(x =Sowing_date, max_yld_t_ha), colour ="red")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_rect(colour = "blue", fill=NA, linewidth=1))+
  
  ylim(0,6)+
  labs(title = "Yield vs dry sowing dates Kyancutta 18170.\nOptimal germination conditions using water balance with threshold of 5mm",
       subtitle = 
       "This is not the optimal germination conditions, but the start of window.
       If optimal germination conditions are not met no gernimation will occur and no yield will be produced.
       Here these years have been replaced with zero yield",
       y = "Yield t/ha",
       x ="Dry sowing date",
       caption = "Red dot = 2023."
       )
plot2_box






ggsave(plot = plot2_box,
       filename = paste0(path_saved_files,"/Box_yield_vs_drySowing_dates_dummy_zero_ylds_Kyancutta", ".png" ),
       width = 20, height = 12, units = "cm")



#################################################################################
### now just the mean value 
str(Yield_with_zeros)

Yield_with_zeros_summary <- Yield_with_zeros %>% 
  group_by(Sowing_date) %>% 
  summarize(mean_yld=mean(max_yld_t_ha), 
            sd_yld=sd(max_yld_t_ha), 
            N_yld=n(), 
            se=sd_yld/sqrt(N_yld), 
            upper_limit=mean_yld+se, 
            lower_limit=mean_yld-se 
  ) 

Yield_with_zeros_summary

ggplot(Yield_with_zeros_summary, aes(x=Sowing_date, y=mean_yld)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit))


plot3_mean_yld <- Yield_with_zeros_summary %>% 
  ggplot(aes(x =Sowing_date, mean_yld))+
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit))+
  
  
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_rect(colour = "blue", fill=NA, linewidth=1))+
  
  #ylim(0,6)+
  labs(title = "Yield vs dry sowing dates Kyancutta 18170.\nOptimal germination conditions using water balance with threshold of 5mm",
       subtitle = 
         "This is not the optimal germination conditions, but the start of window.
       If optimal germination conditions are not met no gernimation will occur and no yield will be produced.
       Here these years have been replaced with zero yield",
       y = "Yield t/ha",
       x ="Dry sowing date",
       # caption = ""
  )

plot3_mean_yld


ggsave(plot = plot3_mean_yld,
       filename = paste0(path_saved_files,"/mean_yield_vs_drySowing_dates_dummy_zero_ylds_Kyancutta", ".png" ),
       width = 20, height = 12, units = "cm")





