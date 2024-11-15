# ==========================================================================
# Clean the Freedom House data
# Written: Yixin Sun
# July, 2023
# ==========================================================================


# ==========================================================================
# Set up
# ==========================================================================
# clean the environment
rm(list = ls())

# load the required packages
pacman::p_load(tidyverse, readxl, janitor, countrycode)

# get the path to the active R file
working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# use this path to laod file paths that will be used throughout the document
source(file.path(dirname(working_dir), "filepaths.R"))

# Set other directories
wdir <- file.path(ddir, "data/WHO_air_quality/raw_data")
odir <- file.path(ddir, "data/WHO_air_quality/use_data")

#==============================================================================
# Read in data
#==============================================================================
# read in data on the Freedom in the World Index 2003-2019
# Note from Martinez: "I download raw data from 
# https://freedomhouse.org/sites/default/files/2020-02/2020_Country_and_Territory_Ratings_and_Statuses_FIW1973-2020.xlsx (last accessed on 12/03/2020)
# Relevant information is in the "Country Ratings, Statuses" sheet, but headers complicate importing into Stata, so I have fixed by hand and saved as FIW.csv
# Periodicity is a bit irregular in the 1980s. e.g., data is missing for 1982 (i.e. divided between 1981 and 1983)"
# use as.data.fram to convert from tibble to datarame
fiw_data_new <- 
  file.path(wdir, "All_data_FIW_2013-2023.xlsx") %>%
  read_excel(sheet = 2, skip = 1) %>% 
  filter(`C/T` == "c") %>%
  select(cntry_name= 1, fiw_pr = 6, fiw_cl = 7, dem = Status, year = Edition) 

fiw_data_old <- 
  file.path(wdir, "FIW.csv") %>%
  read_csv() %>%
  pivot_longer(cols = -cntry_name, names_to = c("type", "year"), 
               names_sep = "_") %>% 
  pivot_wider(id_cols = c("cntry_name", "year"), names_from = "type", values_from = "value") %>%
  rename(fiw_pr = pr, fiw_cl = cl, dem = status) %>%
  mutate_at(vars(year, fiw_pr, fiw_cl), as.numeric) %>%
  anti_join(fiw_data_new)

fiw_data <- bind_rows(fiw_data_new, fiw_data_old)

# read in data on electoral democracies 1989-2015
democracy_data <- as.data.frame(
  file.path(wdir, "FIW_electoral_democracy.csv") %>%
    read_csv(show_col_types = FALSE) 
) 

#==============================================================================
# Process data
#==============================================================================
#-------------------Clean the FWI data (original + updated data)------------------------------------------

# assign an ISO3 country code to each country name
fiw_data$countrycode <- countrycode(fiw_data$cntry_name, 
                                    origin = "country.name", 
                                    destination = "iso3c")

# Manually input empty missing codes
fiw_data$countrycode <- 
  ifelse(fiw_data$cntry_name == "Czechoslovakia", "CZE", fiw_data$countrycode)
fiw_data$countrycode <- 
  ifelse(fiw_data$cntry_name == "Kosovo", "XKX", fiw_data$countrycode)
fiw_data$countrycode <- 
  ifelse(fiw_data$cntry_name == "Micronesia", "FSM", fiw_data$countrycode)
fiw_data$countrycode <- 
  ifelse(fiw_data$cntry_name == "Serbia and Montenegro", "CS", fiw_data$countrycode)
fiw_data$countrycode <- 
  ifelse(fiw_data$cntry_name == "Yugoslavia", "YU", fiw_data$countrycode)

# check if there are remaining countries with missing ISO3 codes (0)
sum(is.na(fiw_data$countrycode))


# replace values that do not match the format (following Martinez)
fiw_data$fiw_pr <- ifelse(fiw_data$fiw_pr == "2(5)", "2", fiw_data$fiw_pr) 
fiw_data$fiw_pr <- ifelse(fiw_data$fiw_pr == "-", NA , fiw_data$fiw_pr)
fiw_data$fiw_cl <- ifelse(fiw_data$fiw_cl == "3(6)", "3", fiw_data$fiw_cl)
fiw_data$fiw_pr <- ifelse(fiw_data$fiw_cl == "-", NA , fiw_data$fiw_cl)

# convert the class of fiw_pr and fiw_cl from character to numeric
fiw_data$fiw_pr <- as.numeric(fiw_data$fiw_pr)
fiw_data$fiw_cl <- as.numeric(fiw_data$fiw_cl)

# check if there are any observations with the same countrycode and year (611)
duplicates <- fiw_data %>% group_by(countrycode, year) %>% filter(n() > 1) 

# collapse by countrycode year 
# sometimes two countrynames have the same code (for example, East Germany and Wrst Germany have the same code: DEU)
fiw_data <- fiw_data %>%
  group_by(countrycode, year) %>%
  summarise(fiw_pr = max(fiw_pr, na.rm=TRUE),
            fiw_cl = max(fiw_cl, na.rm=TRUE), 
            dem = dem[1]) %>%
  ungroup

# save the cleaned fiw data
write.csv(fiw_data, file = file.path(odir, "fiw.csv"), row.names = FALSE)

#-------------------Clean the electoral democracy data (only the original data)--------------------------

# specify the years of the dataframe for the reshape function
years <- 1989:2015
# the last column number that needs to be tranformed wide to long
num_columns <- ncol(democracy_data)
# the expected number of rows in the transformed dataset
num_rows <- nrow(democracy_data)*num_columns

# transform from wide (country-level with year columns) into long (country-year level)
democracy_data <- reshape(data = democracy_data,
                    idvar = "country",
                    varying = 2:num_columns,
                    sep = "",
                    timevar = "year",
                    times = years,
                    new.row.names= 1:num_rows,
                    direction = "long")

# assign an ISO3 country code to each country name
democracy_data$countrycode <- countrycode(democracy_data$country, 
                                    origin = "country.name", 
                                    destination = "iso3c")

# Manually input empty missing codes
democracy_data$country <- 
  ifelse(democracy_data$country == "Czechoslovakia", "CZE", democracy_data$country)
democracy_data$country <- 
  ifelse(democracy_data$country == "Kosovo", "XKX", democracy_data$country)
democracy_data$country <- 
  ifelse(democracy_data$country == "Micronesia", "FM", democracy_data$country)
democracy_data$country <- 
  ifelse(democracy_data$country== "Serbia and Montenegro", "CS", democracy_data$country)
democracy_data$country <- 
  ifelse(democracy_data$country == "Yugoslavia", "YU", democracy_data$country)

# check the unique values taken on by the democracy variable (in case there are "-" or other characters instead of NA)
unique(democracy_data$democracy)

# replace "-" with NA
democracy_data$democracy <- ifelse(democracy_data$democracy == "-", NA , democracy_data$democracy)

# create the indicator variable for autocracy, autocracy = 1 if democracy = NO.
democracy_data$autocracyFH <- ifelse(democracy_data$democracy == "No", 1 , 0)

# drop the democracy variable
democracy_data$democracy <- NULL
   
# rename the country into countryname
democracy_data <- democracy_data %>% rename(countryname = country)

# check if there are any observations with the same country and year (no duplicates)
duplicates <- democracy_data %>% group_by(countrycode, year) %>% filter(n() > 1) 

# collapse by countrycode year
democracy_data <- democracy_data %>%
  group_by(countrycode, year) %>%
  summarise(autocracyFH = mean(autocracyFH, na.rm=TRUE))

# save the cleaned democracy data
write_csv(democracy_data, file = file.path(odir, "democracy.csv"), row.names = FALSE)
