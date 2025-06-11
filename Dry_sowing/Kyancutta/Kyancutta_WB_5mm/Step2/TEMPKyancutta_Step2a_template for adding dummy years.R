# Download Daily climate files created in step 1 (with frost days) -------------------------------------------------------
#"X:\Riskwi$e\Dry_sowing\Kyancutta\Results"
met_frost <- read_csv("X:/Riskwi$e/Dry_sowing/Kyancutta/Results/Fost_details_18170.csv")
str(met_frost)

# Download Daily APSIM output -------------------------------------------------------
Kyancutta_Daily_APSIM <- read_csv("X:/Riskwi$e/Dry_sowing/Kyancutta/APSIM_runs/Kyancutta_Water_balance_v5.Daily.csv", 
                                  col_types = cols(Clock.Today = col_date(format = "%Y-%m-%d")))


names(Kyancutta_Daily_APSIM)
str(Kyancutta_Daily_APSIM)
str(Kyancutta_Daily_APSIM$Clock.Today)
max(Kyancutta_Daily_APSIM$Clock.Today)
min(Kyancutta_Daily_APSIM$Clock.Today)

################################################################################
## subset APSIM data

Kyancutta_Daily_APSIM <- Kyancutta_Daily_APSIM %>% select(
  CheckpointName,
  SimulationName,
  StartDate,
  EndDate,
  Clock.Today ,
  Wheat.Phenology.Zadok.Stage,
  Wheat.Phenology.CurrentStageName,
  Wheat.Phenology.Stage,
  Yield ,
  Yield_Adj_t_ha 
)
## Create a clm with just the years from the date

Kyancutta_Daily_APSIM <- Kyancutta_Daily_APSIM %>% 
  mutate(year = year(Clock.Today))

number_potentail_years <- max(Kyancutta_Daily_APSIM$year) - min(Kyancutta_Daily_APSIM$year)


################################################################################
### not every years triggered a sowing event
Kyancutta_Daily_APSIM %>% distinct(Wheat.Phenology.CurrentStageName)
sowing_germination_event_trigger <- Kyancutta_Daily_APSIM %>% 
  filter(Wheat.Phenology.CurrentStageName == "Sowing") %>% 
  group_by(StartDate) %>% 
  summarise(count_yrs_sowing = n()) %>% 
  mutate(number_potentail_years = number_potentail_years)
sowing_germination_event_trigger

################################################################################
## make a dummy df / template that has all the treatments and all the years.

start_dates <- Kyancutta_Daily_APSIM %>% distinct(StartDate)
start_dates_1 <-dplyr::pull(start_dates, StartDate)


max(Kyancutta_Daily_APSIM$Clock.Today)
min(Kyancutta_Daily_APSIM$Clock.Today)

days <- seq.Date(as.Date("1957-01-01"),as.Date("2024-12-31"), "days")
# check_number_of_day_correct <- Kyancutta_Daily_APSIM %>% filter(StartDate == "1-apr")
# rm(check_number_of_day_correct)

# Create the data frame with all combinations
template <- expand.grid(start_dates_1, days)

# Rename the columns for clarity
colnames(template) <- c("StartDate", "date")

str(template$date)
str(template$StartDate)
str(Kyancutta_Daily_APSIM$Clock.Today)

template$StartDate <- as.character(template$StartDate)
template <- template %>%  rename(Clock.Today = date)
head(template)
template1 <- left_join(template, Kyancutta_Daily_APSIM)

check_april_2015plus <- template1 %>% filter(StartDate == "1-apr") %>% 
  filter(year >2015)
## Make a 2 new clm called 'Sowing_date' that will match the other files and a temp file to work out what to keep


