# ========================================================================
# Clean the WHO reported PM2.5
# Written: Yixin Sun
# July, 2023
# ========================================================================

# ==========================================================================
# Set up
# ==========================================================================

#load the required packages
pacman::p_load(tidyverse, readxl, janitor, ggmap)

# clean the environment
rm(list = ls())

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

# read in 2010 city-level pm2.5 data
city2010_pm2.5<- 
  file.path(wdir, "oap_database.xls") %>%
  read_excel(sheet = "PM2.5 (cities)", skip = 1) %>%
  filter(row_number() != 1)

# read in 2010 country-level pm2.5 data
country2010_pm2.5 <- 
  file.path(wdir, "oap_database.xls") %>%
  read_excel(sheet = "PM2.5 (countries)", skip = 1) %>%
  select(1:8) %>%
  filter(row_number() != 1)

# read in 2010 city-level pm10 data
city2010_pm10 <- 
  file.path(wdir, "oap_database.xls") %>%
  read_excel(sheet = "PM10 (cities)", skip = 1) %>%
  filter(row_number() != 1)

# read in 2010 country-level pm10 data
country2010_pm10 <- 
  file.path(wdir, "oap_database.xls") %>%
  read_excel(sheet = "PM10 (countries)", skip = 1) %>%
  filter(row_number() != 1) 

# read in 2013 city-level pm2.5 and pm10 data
city2013 <- 
  file.path(wdir, "aap_pm_database_may2014.xls") %>%
  read_excel(sheet = "cities", skip = 1)

# read in 2013 country-level pm2.5 and pm10 data
country2013 <- 
  file.path(wdir, "aap_pm_database_may2014.xls") %>%
  read_excel(sheet = "countries", skip = 1) 

# read in 2015 city-level pm2.5 and pm10 data (constructed in 2016)
who2015 <- 
  file.path(wdir, "who-aap-database-may2016.xlsx") %>%
  read_excel(sheet = "database", skip = 2)

# read in 2017 city-level pm2.5 and pm10 data (constructed in 2018)
who2017 <- 
  file.path(wdir, "aap_air_quality_database_2018_v14.xlsx") %>%
  read_excel(sheet = "database", skip = 2)

# read in 2022 city-level pm2.5 and pm10 data
who2022 <- 
  file.path(wdir, "who_aap_2022.xlsx")%>%
  read_excel(sheet = "AAP_2022_city_v9", skip = 0)

# read in 2023 city-level pm2.5 and pm10 data
who2023 <- 
  file.path(wdir, "who_aap_2023_(v6.0).xlsx")%>%
  read_excel(sheet = "Update 2023 (V6.0)", skip = 0)

#==============================================================================
# Process data
#==============================================================================
# process the 2010 city-level pm2.5 data
city2010_pm2.5 <- 
  city2010_pm2.5 %>%
  rename(
    pm2.5 = contains("PM2.5"), pm2.5_monitors = starts_with("Number"), 
    pm2.5_timecoverage = starts_with("Temporal")
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(pm2_5), 
    year = as.numeric(str_extract(year, "([0-9]+)")), 
    databaseyear = 2011, 
    pm2_5_timecoverage = as.numeric(pm2_5_timecoverage)
  ) %>% 
  distinct() %>%
  mutate(
    city = case_when(city == "Zona Met de Guadalajara" ~ "Zona Metropolitana de Guadalajara", 
                     city == "Zona Met de Monterrey" ~ "Zona Metropolitana de Monterrey", 
                     city == "Zona Met del Valle de Mexico" ~ "Zona Metropolitana del Valle de Mexico", 
                     TRUE ~ city)
  )

# process the 2010 country-level pm2.5 data
country2010_pm2.5 <-
  country2010_pm2.5%>%
  rename(
    pm2.5 = contains("PM2.5"), 
    pm2.5_monitors = starts_with("Number"), 
    pm2.5_timecoverage = starts_with('Temporal')
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(pm2_5), 
    year = as.numeric(str_extract(year, "([0-9]+)")), 
    databaseyear = 2011, 
    pm2_5_timecoverage = as.numeric(pm2_5_timecoverage)
  ) %>% 
  distinct() 

# process the 2010 city-level pm10 data
city2010_pm10 <-
  city2010_pm10 %>%
  rename(
    pm10 = contains("PM10"), 
    pm10_monitors = starts_with("Number"), 
    pm10_timecoverage = starts_with('temporal')
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm10 = as.numeric(pm10), 
    year = as.numeric(str_extract(year, "([0-9]+)")), 
    pm10_timecoverage = as.numeric(pm10_timecoverage)
  ) %>% 
  distinct() %>%
  mutate(
    pm10_from_pm2_5 = str_detect(reference, regex("calculated", ignore_case = TRUE))
  ) %>%
  select(-region, -reference)

# process the 2010 country-level pm10 data
country2010_pm10 <-
  country2010_pm10  %>%
  rename(
    pm10 = contains("PM10"), 
    pm10_monitors = starts_with("Number"), 
    pm10_timecoverage = starts_with('Temporal')
  ) %>%
  janitor::clean_names() %>%
  select(1:9) %>%
  mutate(
    pm10 = as.numeric(pm10), 
    year = as.numeric(str_extract(year, "([0-9]+)")), 
    pm10_timecoverage = as.numeric(pm10_timecoverage), 
    city = "mean"
  ) %>% 
  distinct() %>%
  mutate(
    pm10_from_pm2_5 = str_detect(reference, regex("calculated", ignore_case = TRUE))
  ) %>%
  select(-region, -reference)

# merge the city pm2.5 and pm10 datasets, 
# merge the country pm2.5 and pm10 datasets,
# append the city and country datasets
who2010 <- 
  full_join(
    city2010_pm2.5, 
    city2010_pm10, 
    by = c("city", "country", "year")
  ) %>%
  bind_rows(
    full_join(
      country2010_pm2.5, 
      country2010_pm10, 
      by = c("city", "country", "year")
    )
  ) %>%
  mutate(databaseyear = 2011)

# clean the city2013 pollutant dataset
city2013 <-
  city2013  %>%
  rename(
    monitors = starts_with("of monitoring"), 
    timecoverage = starts_with('Temporal'), 
    city = starts_with ("City"), 
    pm10 = 5, 
    pm10_year = 6, 
    pm2.5 = 8, 
    pm2.5_year = 9, 
    reference = `for air quality`
  ) %>%
  janitor::clean_names() %>%
  filter(!is.na(country)) %>%
  mutate(
    pm2_5 = as.numeric(pm2_5), 
    year = if_else(!is.na(pm2_5_year), 
                   pm2_5_year, 
                   as.numeric(str_extract(pm10_year, "([0-9]+)"))
    ),
    databaseyear = 2014, 
    timecoverage = as.numeric(timecoverage), 
    pm10_from_pm2.5 = note_on_converted_pm10 > 0, 
    pm2_5_from_pm10 = note_on_converted_pm2_5 > 0) %>%
  select(-pm2_5_year, -pm10_year, -x14, -x15)

# clean the country2013 pollutant dataset
country2013 <- 
  country2013 %>%
  rename(
    monitors = starts_with("of monitor"), 
    pm10 = 5, 
    pm10_year = 6, 
    pm2.5 = 3,
    pm2.5_year = 4, 
    reference = `for air quality`) %>%
  janitor::clean_names() %>%
  filter(!is.na(country)) %>%
  mutate(
    pm2_5 = as.numeric(pm2_5), 
    year = if_else(!is.na(pm2_5_year), 
                   as.numeric(str_extract(pm2_5_year, "([0-9]+)")), 
                   as.numeric(str_extract(pm10_year, "([0-9]+)"))
    ),
    databaseyear = 2014, 
    city = "Mean"
  ) %>%
  select(-pm2_5_year, -pm10_year)

# append the 2013 city and country datasets
who2013 <- bind_rows(country2013, city2013)

# clean the who2015 dataset
who2015 <-
  who2015 %>% 
  rename(
    pm10 = `Annual mean, ug/m3...6`, 
    pm10_year = `Year...7`,
    pm2.5 = `Annual mean, ug/m3...10`, 
    pm2.5_year = `Year...11`, 
    pm10_monitors = contains("(PM10)"), 
    pm2.5_monitors = contains("(PM2.5)"),
    timecoverage = starts_with('Temporal'), 
    city = starts_with ("City"), 
    reference = `for air quality`
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(pm2_5), 
    year = if_else(!is.na(pm2_5_year), 
                   as.numeric(str_extract(pm2_5_year, "([0-9]+)")), 
                   as.numeric(str_extract(pm10_year, "([0-9]+)"))
    ),
    databaseyear = 2016, 
    timecoverage = as.numeric(timecoverage), 
    pm10_from_pm2.5 = note_on_converted_pm10 == "converted from PM2.5", 
    pm2_5_from_pm10 = note_on_converted_pm2_5 == "converted from PM10"
  ) %>%
  select(-pm2_5_year, -pm10_year,-starts_with("note"))

# clean the who2017 dataset
who2017 <-
  who2017  %>%
  rename(
    pm10 = `Annual mean, ug/m3...6`, 
    pm2.5 = `Annual mean, ug/m3...9`, 
    monitors = contains("Number"), 
    pm2.5_timecoverage_discrete = `Temporal coverage...10`, 
    pm10_timecoverage_discrete = `Temporal coverage...7`,
    city = starts_with ("City"), 
    reference = `Reference for air quality`, 
    databaseyear = starts_with("Database")
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(str_extract(pm2_5, "([0-9]+)")), 
    pm10 = as.numeric(str_extract(pm10, "([0-9]+)")),
    pm10_from_pm2.5 = note_on_converted_pm10 == "Converted", 
    pm2_5_from_pm10 = str_detect(note_on_converted_pm2_5, regex("converted", ignore_case = TRUE))
  ) %>%
  select(-starts_with("note"))

# clean the who2022 dataset
who2022 <-
  who2022 %>%
  rename(
    region = contains("Region"),
    country = contains("Country"),
    city = starts_with ("City"),
    year = `Measurement Year`,
    pm10 = `PM10 (μg/m3)`, 
    pm2.5 = `PM2.5 (μg/m3)`, 
    monitors = contains("monitoring stations"), 
    pm2.5_timecoverage = `PM25 temporal coverage (%)`, 
    pm10_timecoverage = `PM10 temporal coverage (%)`,
    reference = `Reference`, 
    databaseyear = contains("database")
  ) %>%
  select(-contains('NO2')) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(str_extract(pm2_5, "([0-9]+)")), 
    pm10 = as.numeric(str_extract(pm10, "([0-9]+)")),
    pm2_5_timecoverage = as.numeric(pm2_5_timecoverage),
    pm10_timecoverage = as.numeric(pm10_timecoverage)
  ) 

# clean the who2023 dataset
who2023 <-
  who2023 %>%
  rename(
    region = who_region,
    country = country_name,
    databaseyear = version,
    pm10 = pm10_concentration,
    pm2_5 = pm25_concentration,
    pm10_timecoverage = pm10_tempcov,
    pm2_5_timecoverage = pm25_tempcov,
    monitors = type_of_stations
  ) %>%
  janitor::clean_names() %>%
  mutate(
    pm2_5 = as.numeric(str_extract(pm2_5, "([0-9]+)")), 
    pm10 = as.numeric(str_extract(pm10, "([0-9]+)")),
    pm2_5_timecoverage = as.numeric(pm2_5_timecoverage),
    pm10_timecoverage = as.numeric(pm10_timecoverage),
    databaseyear = as.numeric(substr(str_extract(databaseyear, "([0-9]+)"), 1, 4)) 
  ) %>%
  select(-no2_concentration, -no2_tempcov, -web_link, -population,
         -population_source, -latitude, -longitude, -who_ms)

#==============================================================================
# append all datasets together 
#==============================================================================
who <- 
  list(
    who2010, 
    who2013,
    who2015, 
    who2017,
    who2022,
    who2023
  ) %>%
  reduce(bind_rows) %>%
  mutate(
    pm2_5_from_pm10 = if_else(is.na(pm2_5) & databaseyear == 2011, 
                              0, pm2_5_from_pm10),
    reference = replace(reference, reference %in% c("NA", "-"), NA),
    country = str_to_upper(country),
    city = str_to_upper(city),
    country = case_when(
      country == "VIET NAM" ~ "VIETNAM",  
      str_detect(country, "VENEZUELA") ~ "VENEZUELA", 
      str_detect(country, "TANZANIA") ~ "TANZANIA", 
      str_detect(country, "ISRAEL") ~ "ISRAEL", 
      str_detect(country, "IRAN") ~ "IRAN", 
      str_detect(country, "BOLIVIA") ~ "BOLIVIA", 
      str_detect(country, "KOREA") ~ "SOUTH KOREA", 
      str_detect(country, "RUSSIA") ~ "RUSSIA", 
      str_detect(country, "MYANMAR") ~ "BURMA", 
      str_detect(country, "GAMBIA") ~ "GAMBIA",
      str_detect(country, "BOLIVIA") ~ "BOLIVIA", 
      str_detect(country, "ENGLAND") ~ "UNITED KINGDOM",
      str_detect(country, "BRUNEI") ~ "BRUNEI",
      str_detect(country, "UNITED STATES") ~ "UNITED STATES", 
      str_detect(country, "CHINA") ~ "CHINA", 
      TRUE ~ country)
  ) %>% 
  mutate(num_vars = as.numeric(!is.na(pm2_5)) + as.numeric(!is.na(pm10))) %>%
  group_by(year, country, city, reference) %>%
  arrange(-num_vars, -databaseyear, desc(nchar(pm2_5))) %>%
  filter(row_number() == 1)  
# Here I prioritize dups that have more data, are from 
# the most recent database, and give the most precise pm measurements 

# clean the environment from datasets that we no longer need
# rm(who2010, who2013, who2015, who2017, who2022, who2023, country2013, city2013, 
#    city2010_pm10, city2010_pm2.5, country2010_pm10, country2010_pm2.5)

# strings to detect a local source among the references
local_regex <- c('European Environment',
                 'Environmental Protection Agency',
                 'Environment Protection Authority',
                 'Environmental Protection Authority',
                 'Environment Agency',
                 'Department of Environment',
                 'Environment Department',
                 'Environment Bureau',
                 'Environment of Tokyo',
                 'Supreme Council',
                 'Health Department',
                 'EPA',
                 'Minist',
                 'Minsitry',
                 'Government',
                 'government',
                 '.gov.',
                 'Secretaria',
                 'Agencia',
                 'Nacional',
                 'Estado',
                 'National',
                 'Western Australia',
                 'Tasmania',
                 'South Australia',
                 'ACT Air Quality',
                 'AirKorea',
                 'Department of Environment and Natural',
                 'Central Environment',
                 'Departamento Administrativo',
                 'Annual report on air quality',
                 'Area Metropolitana',
                 'CPCB',
                 'Central Pollution Control Board',
                 'Bureau, Pollution Control',
                 'Bureau of Statistics',
                 'State-of-Environment',
                 'State of Environment',
                 'redmonica',
                 'RED MoniCA',
                 'RED Monica',
                 'Plataforma',
                 'Instituto Brasileiro',
                 'Relatório annual',
                 'Relatorio annual',
                 'Subsistema de Informacion sobre Calidad del Aire',
                 'Fundação Estadual',
                 'Monitoramento da Qualidade do',
                 'Municipality',
                 'Official',
                 'Council',
                 'Gobierno',
                 'Manchester',
                 'Nova Scotia',
                 'Alberta',
                 'Manitoba',
                 'Prince Edward',
                 'Newfoundland',
                 'Environment Canada',
                 'Buenos Aires Ciudad',
                 'Calidad del Aire',
                 'Air quality domain report',
                 'Department of Statistics',
                 'System of Kuwait',
                 'State of Kuwait',
                 'Statistics Office',
                 'Karnataka State Pollution Control Board',
                 'Hungarian Mete',
                 'South African Air Quality Information',
                 'Control Department',
                 'Compendio',
                 'Boletin',
                 'Aire CDMX',
                 'BAFU',
                 'Informe',
                 'Directive',
                 'Direction',
                 'Dirección',
                 'Dipartimento',
                 'Bangkok Ambient Annual Summary',
                 'Environmental Statistics',
                 'Statistics Centre',
                 'cuencaire',
                 'Andorra',
                 'Finnish Meteorological Institute',
                 'Sistema Integral',
                 'Clean air asia',
                 'Clean Air Asia',
                 'Clear Air Asia',
                 'ClearnAir Asia',
                 'Clean Air Ini',
                 "L'environnement en principauté",
                 'Observatorio Ambiental',
                 'Boletín de calidad',
                 'Air Korea',
                 'State of the Environment',
                 'the state of the environment',
                 'CDMX cuidad de mexico, SEDEMA',
                 'pcd.go.th',
                 'Tripoli Environment and Development Observatory',
                 'DLI',
                 'O.M.Marzeiev Institute for Public Health of NAMSU',
                 'NEPM',
                 'Hong Kong Air Quality Health Index',
                 'Hydrometeorological Institute',
                 'Republici',
                 'Central Organisation for Statistics',
                 'Environment Public Authority',
                 'NAMEM',
                 'MfE',
                 'Environmental Management Authority',
                 'Automatic Urban and Rural Network',
                 'Municibality',
                 'Environmental agency -AbuDhabi',
                 'NCM',
                 'Municibalitiy',
                 'Red de monitoreo de particulas suspendidas en el aire en Tegucigalpa, 2013-2014',
                 'Aotearoa',
                 'Central Organization of Statistics & Information',
                 'Environnement Protection Authority',
                 'Northern Territory Ambient Air Quality Monitoring',
                 'klab.ee',
                 'Institut Monégasque de la Statistique',
                 'Bahrain Information Data Portal',
                 'Statistics Mauritius',
                 'Direccion  General de Salud Ambiental e Inocuidad',
                 'MoE',
                 'SAARC',
                 'Suivi de la',
                 'Environment of Peru'
) %>%
  paste(collapse = "|") %>%
  regex(ignore_case = TRUE)

# strings to detect a non-local source among the references
other_regex <- c('Journal',
                 'impact assessment',
                 'AirNow',
                 'et al',
                 'J. Antonel, Z.',
                 'African Review of Phys',
                 'Brown KW',
                 'SAFAR',
                 'Safar',
                 'spartan',
                 'script',
                 'Presentation',
                 'Atmospheric Pollution Research',
                 'Environ Monit Assess.',
                 '408(6)',
                 'j.env',
                 'Paper',
                 'Air Quality Standards for Particulate Matter',
                 'Measurements and analysis of air quality in Islamabad',
                 'Sci. Total Environ',
                 'Air Quality Standards for',
                 'IEMA',
                 'PAHO',
                 'GBD project',
                 'Exposure levels of air pollution',
                 'Report of Current Situation of PM2.5 in Vietnam',
                 'Plan de accion para combustible y vehiculos',
                 'asialeds.org'
) %>%
  paste(collapse = "|") %>%
  regex(ignore_case = TRUE)

# classify the references as a local source (1) or other (0)
who <- 
  who %>%
  mutate(
    localsource = case_when(str_detect(reference, local_regex) ~ 1, 
                            str_detect(reference, other_regex) ~ 0, 
                            TRUE ~ NA_real_), 
    airnow = str_detect(reference, regex("airnow|u.s. dep", ignore_case = TRUE)))

# display the references and local sources next to each other to check them
check_reference <- select(who, country, city, year, databaseyear, reference, localsource)


# detect observations with a missing reference (1642)
missing_reference <- filter(who, is.na(reference))

# detect observations with non-missing reference, but missing local source
missing_localsource <- filter(check_reference, is.na(localsource)&!is.na(reference)) 

# make sure data is unique at year-country-city-localsource level 
# Again I prioritize dups that have more data, are from 
# the most recent database, and give the most precise pm measurements 
who <- 
  who %>%
  group_by(year, country, city, localsource) %>%
  arrange(-num_vars, -databaseyear, desc(nchar(pm2_5))) %>%
  filter(row_number() == 1)  %>%
  ungroup 

# if there are multiple observations with the same year, country, city, but one has a missing localsource, 
# then keep only the observation with non-missing localsource
who <- who %>%
  group_by(year, country, city) %>%
  filter(n() == 1 | complete.cases(localsource)) %>%
  ungroup

# check that the dataset is unique at the city-country-year-localsource level
sum(duplicated(paste(who$year, who$country, who$city, who$localsource)))

# check if there are remaining observations with same year, country, city (62)
duplicates <- 
  who %>%
  group_by(year, country, city)%>%
  filter(n() > 1) 

#==============================================================================
# geocode using google maps to get centroid coordinates of the city
#==============================================================================
register_google(key = "AIzaSyB4rT7aH1TQFGoFID-Sbo6yOfZ1jPWOcaQ", write = TRUE)

who_geocode <- 
  who %>%
  mutate(city = if_else(city == "MEAN", NA_character_, city)) %>%
  mutate(geocode_name = paste0(city, ", ", country)) %>%
  select(city, country, geocode_name) %>%
  distinct() %>%
  mutate_geocode(location = geocode_name, output = "latlona")


who <- 
  full_join(who_geocode, who) %>%
  mutate(lon = case_when(geocode_name == "NOCERA INFERIORE, ITALY" ~ 14.636553221468155, 
                         geocode_name %in% c("BALDEVI (DADRA & NAGAR HAVELI)*, INDIA", "BALDEVI (DADRA & NAGAR HAVELI), INDIA") ~ 73.07373381018175, 
                         geocode_name == "SAINT-PAUL, FRANCE" ~ 7.1208139258426, 
                         geocode_name == "NOCERA INFERIORE, ITALY" ~ 14.638613157976057, 
                         geocode_name == "JIANGZHOU QU, CHINA" ~ 107.41893984396569, 
                         TRUE ~ lon), 
         lat = case_when(geocode_name == "NOCERA INFERIORE, ITALY" ~ 40.75716232441473, 
                         geocode_name %in% c("BALDEVI (DADRA & NAGAR HAVELI)*, INDIA", "BALDEVI (DADRA & NAGAR HAVELI), INDIA")~ 20.149957584002664, 
                         geocode_name == "SAINT-PAUL, FRANCE" ~ 43.69719401487449, 
                         geocode_name == "JIANGZHOU QU, CHINA " ~ 22.561036278903043, 
                         geocode_name == "NOCERA INFERIORE, ITALY" ~ 40.75404151586773, 
                         TRUE ~ lat))

check <- filter(who, is.na(lon))


saveRDS(who, file.path(ddir, "/generated_data/who.rds"))