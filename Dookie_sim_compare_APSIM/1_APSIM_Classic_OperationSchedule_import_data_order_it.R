# This file is for importing APISM sim files.
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)

### create a list of files
### create a list of headers
### put file name into df

#################################################################################

# Daily output files -------------------------------------------------------

site <-"/Operation_schedule"

file_directory <- paste0("X:/Riskwi$e/Dookie/2_Sims_post_Sep2024/1_Classic",site)

file_directory

################################################################################
list_sim_out_file <-
  list.files(
    path = file_directory,
    pattern = "Daily.out",
    all.files = FALSE,
    full.names = FALSE
  )
list_sim_out_file



list_sim_out_file <- c(
  "Operation_schedule Nil outputfileDaily.out",
  "Operation_schedule N_BankProfit outputfileDaily.out",      
  "Operation_schedule N_BankYield outputfileDaily.out"  ,     
  "Operation_schedule National_Av outputfileDaily.out" ,      
  "Operation_schedule N_BankConservative outputfileDaily.out"  
)
                         
list_sim_out_file


#Test approach
#list_sim_out_file <- c("Operation_schedule Nil outputfileDaily.out")
# create a empty data frame -----------------------------------------------

df_for_all_data <- data.frame(
  dd_mm_yyyy		= character(),
  `sw()`          = double(),
  Rotation_N0.Wheat.biomass 	= double(),
  Rotation_N0.barley.biomass = double(),
  Rotation_N0.canola.biomass = double(),
  `no3()`	= double(),
  
  Rotation_N0.Wheat.zadok_stage = double(),
  Rotation_N0.barley.zadok_stage = double(),
  Rotation_N0.canola.stage = double(),
  
  Rotation_N0.Wheat.dlt_dm = double(),
  Rotation_N0.barley.dlt_dm = double(),
  Rotation_N0.canola.dlt_dm = double(),
  
  Rotation_N0.Wheat.yield = double(),
  Rotation_N0.barley.yield = double(),
  Rotation_N0.canola.yield = double(),
  
  Rain = double(),	
  
  Rotation_N0.Wheat.swdef_photo = double(),
  Rotation_N0.barley.swdef_photo = double(),
  Rotation_N0.canola.swdef_photo = double(),
  
  Rotation_N0.Wheat.nfact_photo = double(),
  Rotation_N0.barley.nfact_photo = double(),
  Rotation_N0.canola.nfact_photo = double(),
  
  APSIM_Version 		= character(),
  title		= character()
)


### set the heading names for the data you want to import #The order is important
### heading names for file 2,3,5
heading <- c("dd_mm_yyyy",
             "SW",
             "Wheat_biomass",
             "Barley_biomass",
             "canola_biomass",
             
             "NO3",
             "Wheat.zadok_stage",
             "barley.zadok_stage",
             #"canola.zadok_stage", #no such thing only plant growth stages
             
             
             "Wheat_dlt_dm",
             "barley_dlt_dm",
             "canola_dlt_dm",
             
             "Wheat_yield",
             "Barley_yield",
             "Canola_yield",
             
             "Rain",
             "Wheat_sw_stress_photo",
             "barley_sw_stress_photo",
             "canola_sw_stress_photo",
             
             "Wheat_n_stress_photo",
             "barley_n_stress_photo",
             "canola_n_stress_photo",
             
             "Wheat.stage",
             "barley.stage",
             "canola.stage",
             
             
             "APSIM_Version ",
             "title")




# As a Loop ---------------------------------------------------------------
## worked example
#list_sim_out_file <- "Bute Rotation_YO.out"


for (list_sim_out_file in list_sim_out_file){
  
  
  
  df <- read_table(paste0(file_directory,"/",
                          list_sim_out_file ), #change this back to list when your ready to run as loop (list_sim_out_file OR file_name
                   col_names = FALSE, skip = 4)
  
  
  colnames(df)<- heading # add the column headings (as set above - before the loop)
  
  ### pull out the simulation infomation so I can create some new clms
  
  version = read.csv(paste0(file_directory,"/",
                            list_sim_out_file), skip = 0, header = F, nrows = 1, as.is = T)
  
  
  title_a = read.csv(paste0(file_directory,"/",
                            list_sim_out_file), skip = 1, header = F, nrows = 1, as.is = T)
  
  
  ### formatting the above information
  APSIM_version <- version[1,1]
  APSIM_version<-gsub("ApsimVersion = ","",as.character(APSIM_version))
  APSIM_version
  
  title = title_a[1,1]
  title_a<-gsub("Title = ","",as.character(title))
  title_a
  title_b<-gsub(" outputfileDaily","",as.character(title_a)) #daily output remove this
  title_b
  title_c<-gsub("Operation_schedule ","",as.character(title_b)) #this is the leading sim name - will need to change for different sites
  title_c
  title_d <- trimws(title_c) #trim the white space
  title_d
  
  ### get sim settings into the df
  df <- df %>% 
    mutate( APSIM_Version = APSIM_version,
            title = title_d
    )
  names(df)
  
  
  df_for_all_data <- rbind(df_for_all_data, df)
  
}



# Make a bit neater -------------------------------------------------------
rm(list=ls()[ls()!= "df_for_all_data"])

str(df_for_all_data)
names(df_for_all_data)

df_for_all_data_1 <- df_for_all_data %>%
  separate(dd_mm_yyyy,
           into = c("dd", "mm", "Year"),
           remove = FALSE) %>%
  select(
    dd_mm_yyyy,
    Year,
    soil_water = SW,
    # soil_water_start =   startSim_sw,
    # soil_water_sowing =  sow_sw,
    # soil_water_harvest = harv_sw,
    
    soil_NO3 = NO3,
    # soil_NO3_start =   startSim_NO3,
    # soil_N03_sowing =  sow_NO3,
    # soil_NO3_harvest = harv_NO3,
    
    # soil_NH4_start =   startSim_NH4,
    # soil_NH4_sowing =  sow_NH4,
    # soil_NH4_harvest = harv_NH4,
    
    Wheat_biomass,
    Barley_biomass,
    canola_biomass,
    
    Wheat_yield,
    Barley_yield,
    Canola_yield,
    
    Wheat_sw_stress_photo,
    barley_sw_stress_photo,
    canola_sw_stress_photo,
    
    Wheat_n_stress_photo, 
    barley_n_stress_photo, 
    canola_n_stress_photo, 
    
    Wheat.zadok_stage,
    barley.zadok_stage,
    canola.stage,
    
    Wheat_dlt_dm, 
    barley_dlt_dm, 
    canola_dlt_dm, 
    
    Rain,
    title
  )


str(df_for_all_data_1)

#### what years what crops and other info

#Add some clm
str(df_for_all_data_1)

df_for_all_data_1 <- df_for_all_data_1 %>% mutate(
  Source = "APISM_Classic_Operation_Sch",
  Treatment = title,
  Crop = case_when(
    Year  == 2022 ~ "canola",
    Year  == 2023 ~ "Wheat"),
  Cultivar = case_when(
    Year  == 2022 ~ "early",
    Year  == 2023 ~ "yitpi" ),
  Biomass = case_when(
    Year  == 2022 ~ canola_biomass,
    Year  == 2023 ~ Wheat_biomass),
  Yield = case_when(
    Year  == 2022 ~ Canola_yield/1000,
    Year  == 2023 ~ Wheat_yield/1000 ),
    
  sw_stress_photo= case_when(
    Year  == 2022 ~ canola_sw_stress_photo,
    Year  == 2023 ~ Wheat_sw_stress_photo ),
    
  n_stress_photo= case_when(
    Year  == 2022 ~ canola_n_stress_photo,
    Year  == 2023 ~ Wheat_n_stress_photo ),
    
  zadok_stage = case_when(
    Year  == 2022 ~ canola.stage,
    Year  == 2023 ~ Wheat.zadok_stage),
  
  dlt_dm = case_when(
    Year  == 2022 ~ canola_dlt_dm,
    Year  == 2023 ~ Wheat_dlt_dm ),
    
  HarvestIndex = (Yield*1000)/Biomass
  
)

#write.csv(df_for_all_data_1 , "X:/Riskwi$e/Curyo/2_Sims_post_Sep2024/To_compare_etc/TEST1_APSIM_Classic_OperationScheduleDaily.csv", row.names = FALSE )


# drop some clms
str(df_for_all_data_1)
names(df_for_all_data_1)

df_for_all_data_1 <- df_for_all_data_1 %>% select(
  "dd_mm_yyyy" ,
  "Year",
  "Source",
  "Treatment",
  
  "Crop",
  "Cultivar",
  
  "soil_water",
  "soil_NO3"  ,
  
  # "soil_water_start" ,  "soil_water_sowing",    "soil_water_harvest",
  # "soil_NO3_start" ,    "soil_N03_sowing",      "soil_NO3_harvest", 
  # "soil_NH4_start",     "soil_NH4_sowing",      "soil_NH4_harvest" ,
  # "Soil_mineral_N_sowing",
  "Biomass",            "Yield",  
  "sw_stress_photo" , "n_stress_photo",
  "zadok_stage" , "dlt_dm", "HarvestIndex", "Rain"
  
)

unique(df_for_all_data_1$Treatment)

write.csv(df_for_all_data_1 , "X:/Riskwi$e/Dookie/2_Sims_post_Sep2024/To_compare_etc/APSIM_Classic_OperationScheduleDaily.csv", row.names = FALSE )

