# ==========================================================================
# Purpose: Join satellite and government reported WHO data with FIW democracy index 
# Written by: Yixin Sun
# Last updated: 3/15/2024
# ==========================================================================

# ==========================================================================
# Set up
# ==========================================================================
# clean the environment
rm(list = ls())

# install the required packages
# package for conversion from coordinates to UTM
# install.packages("rgdal")

# load the required packages
pacman::p_load(raster, tidyverse, sf, exactextractr, lubridate, terra, countrycode)

# get the path to the active R file
working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# use this path to laod file paths that will be used throughout the document
source(file.path(dirname(working_dir), "filepaths.R"))

# Set other directories
wdir <- file.path(ddir, "data/WHO_air_quality/raw_data")
odir <- file.path(ddir, "data/WHO_air_quality/use_data")


# Directories
rdir <- file.path(ddir, "data")

#==============================================================================
# Read in data
#==============================================================================

# open the rds file saved from clean_who.R
who <- readRDS(file.path(ddir, "generated_data", "who.rds"))

# check summary and missing values (205 observations have missing coordinates)
summary(who)

# read in the raster pm2.5 satellite data for years 2000-2021
for (year in 2000:2021) {
  file_name <- paste0("V5GL03.HybridPM25.Global.", year, "01-", year, "12.nc")
  data <- raster(file.path(rdir, "satellite_pm", file_name), package = 'exactextractr')
  df_name <- paste0("vd_raster", year)
  assign(df_name, data)
  }

# # read in the shape file
# states <-
#   st_read(file.path(rdir, "ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")) %>%
#   dplyr::select(adm1_code, iso_a2, name, name_alt, type, adm0_a3,
#                 admin, woe_label, woe_name)

# read in the data on population
population <- raster(file.path(rdir, "GPW/gpw-v4-population-count-rev11_2010_2pt5_min_tif/gpw_v4_population_count_rev11_2010_2pt5_min.tif"))

#==============================================================================
# Process data
#==============================================================================

#-------------------Clean the coordinates data----------------------------------

# check the observations with missing coordinates
missing_coordinates <- filter(who, is.na(lon)|is.na(lat))

# fill in the missing coordinates
who$lat[which(who$geocode_name=="LE ROBERT, FRANCE")]<-"14.67917"
who$lon[which(who$geocode_name=="LE ROBERT, FRANCE")]<-"-60.94028"

who$lat[which(who$geocode_name=="HARLINGTON, UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND")]<-"51.4859"
who$lon[which(who$geocode_name=="HARLINGTON, UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND")]<-"-0.4364"

who$lat[which(who$geocode_name=="HOLLISTER, UNITED STATES")]<-"36.8525"
who$lon[which(who$geocode_name=="HOLLISTER, UNITED STATES")]<-"-121.4017"

who$lat[which(who$geocode_name=="HAIBEI PREFECTURE, CHINA")]<-"36.9678"
who$lon[which(who$geocode_name=="HAIBEI PREFECTURE, CHINA")]<-"100.902"

who$lat[which(who$geocode_name=="ALI, CHINA")]<-"30.4005"
who$lon[which(who$geocode_name=="ALI, CHINA")]<-"81.1454"

who$lat[which(who$geocode_name=="SYRACUSE (NY), UNITED STATES")]<-"43.0481"
who$lon[which(who$geocode_name=="SYRACUSE (NY), UNITED STATES")]<-"76.1474"

who$lat[which(who$geocode_name=="HILAL, TURKEY")]<-"38.4253"
who$lon[which(who$geocode_name=="HILAL, TURKEY")]<-"27.1544"

who$lat[which(who$geocode_name=="JUNCTION, INDIA")]<-"28.6614"
who$lon[which(who$geocode_name=="JUNCTION, INDIA")]<-"77.2279"

who$lat[which(who$geocode_name=="DIEKIRCH, LUXEMBOURG")]<-"49.8672"
who$lon[which(who$geocode_name=="DIEKIRCH, LUXEMBOURG")]<-"6.1596"

# check the remaining observations with missing coordinates
# (the remaining observations are country level pollution measurements)
missing_coordinates <- filter(who, is.na(lon)|is.na(lat))

# drop the observations with missing values
who <- who[complete.cases(who$lat, who$lon), ]

# convert the class of coordinates from character to numeric
who$lon = as.numeric(who$lon)
who$lat = as.numeric(who$lat)

#-------------------Draw circles around the city centers (the WHO data)---------

who_sf <-
  who %>%
  filter(!is.na(lon)) %>%
  dplyr::select(city, country, lon, lat) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# turn sf into a terra vector object
# cities shouldn't change locations over time so we can create just a cross-sectional dataset
who_terra <- as(who_sf, "Spatial")
who_terra <- vect(who_terra)

# specify the radii for the polygons
radii <- c(5, 10, 20, 50)

# for loop to create polygons of radius 5, 10, 20, 50 km
for (radius in radii) {
  r = radius*1000
  polygon <- who_terra %>%
    terra::buffer(r) %>%
    st_as_sf() %>%
    mutate(area = st_area(geometry),
           radius = sqrt(area / pi))
  polygon_name <- paste0("polygon", radius, "km")
  assign(polygon_name, polygon)
}

# check the crs of satellite data and the polygons: both are WGS 84
st_crs(polygon5km)
crs(vd_raster2021)

#-------------------Satellite data----------------------------------------------

# re-weight pollution data by population
# just do this with one year's pollution data since the point is just to get the
# raster object crs to match up
# resampled_raster <- terra::resample(population, vd_raster2015, "bilinear")
# rf <- writeRaster(resampled_raster, filename = file.path(ddir, "generated_data/satellite_pm/population_resampled.tiff"),
#                   format="GTiff", overwrite=TRUE)
resampled_raster <- raster(file.path(ddir, "generated_data/satellite_pm/population_resampled.tiff"), package = 'exactextractr')


# aggregate satellite data at the city level for varying radii
for (year in 2000:2021) {
  raster_name <- paste0("vd_raster", year)
  gc()

  pm_radius <- st_set_geometry(who_sf, NULL)

  # extract the data on pollution that corresponds to the polygons
  for (radius in radii){
    start <- Sys.time()

    polygon_name = paste0("polygon", radius, "km")

    out <- exact_extract(get(raster_name), get(polygon_name), "weighted_mean",
                         weights = resampled_raster, default_weight = 0)

    # dataset for the current year
    pm_radius <-
      pm_radius %>%
      mutate(!!paste0("pm2.5_", radius, "km") := out)  %>%
      mutate(year := !!year)

    write_csv(pm_radius, file.path(ddir, "generated_data/satellite_pm", paste0("pm_radius_", year, ".csv")))
    print(paste(year, radius, Sys.time() - start))
  }
}

# change from wide to long
pm_radius <- map_df(2008:2021, ~read_csv(file.path(ddir, "generated_data/satellite_pm", paste0("pm_radius_", .x, ".csv"))))

# join this information back with the who data
pollution_data <- 
  who %>% 
  filter(localsource == 1) %>%
  full_join(pm_radius) %>%
  filter(!is.na(city))


#-------------------Combine with FIW and polity data----------------------------------------------
stopifnot(pollution_data %>% group_by(city, country, year, localsource) %>% filter(n() > 1) %>% nrow() == 0)

# check if all were converted: no missing 
sum(is.na(pollution_data$countrycode))

# FWI data
fiw <- 
  read_csv(file.path(ddir, "generated_data/freedom_world_index/fiw.csv")) %>%
  rowwise() %>%
  mutate(fiw = min(fiw_pr, fiw_cl, na.rm = TRUE) - 1) %>%
  mutate(fiw_pr = fiw_pr -1, 
         dem = case_when(fiw_pr < 2 ~ "Free", 
                         fiw_pr >= 2 & fiw_pr <= 4 ~ "Partially Free", 
                         fiw_pr > 4 ~ "Not Free"),
         dem = factor(dem, levels = c("Free", "Partially Free", "Not Free"))) %>%
  filter(year >= 2008)


# merge with FIW data 
reg_data <- 
  pollution_data %>%
  mutate(countrycode = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  filter(year >= 2008) %>% 
  full_join(fiw, by = c("countrycode", "year")) %>%
  left_join(polity) %>%
  group_by(city, countrycode, year) %>%
  arrange(-localsource, -databaseyear) %>%
  filter(row_number()==1) %>%
  ungroup %>%
  mutate(country = countrycode(countrycode, origin = "iso3c", destination = "country.name"))

stopifnot(group_by(reg_data, countrycode, city, year) %>% filter(n() > 1) %>% nrow() == 0)


saveRDS(reg_data, file = file.path(ddir, "generated_data/reg_data.RDS"))

