# ==========================================================================
# Purpose: Create plots and maps describing the PM2.5 and FIW data
# Last updated: 3/12/2024
# ==========================================================================

# ==========================================================================
# Set up
# ==========================================================================
# clean the environment
rm(list = ls())

# load the required packages
pacman::p_load(tidyverse, countrycode, ggplot2, fixest, ggpmisc, knitr, patchwork, broom)

# get the path to the active R file
working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# use this path to laod file paths that will be used throughout the document
source(file.path(working_dir, "filepaths.R"))



#==============================================================================
# Read and clean data
#==============================================================================
# read in data
# get rid of datapoints where the difference between satellite and WHO is outrageously large
reg_data <- 
  readRDS(file.path(ddir, "generated_data", "reg_data.RDS")) %>%
  mutate(pm_diff = pm2_5 - pm2.5_20km, 
         china = country == "China", 
         country = countrycode(countrycode, "iso3c", "country.name")) %>%
  filter(!is.na(fiw_pr), year >= 2008, (abs(pm_diff) <= quantile(abs(pm_diff), .999, na.rm = TRUE) | is.na(pm_diff))) %>%
  group_by(countrycode) 

# for graphs plotting pm2.5, restrict only to areas that have both 
# satellite and WHO pm2.5 datapoints available
desc_data <- 
  reg_data %>%
  filter(!is.na(pm2_5), !is.na(pm2.5_20km)) 


#==============================================================================
# Graph of china's data by year
#==============================================================================
# graph China's satellite vs WHO reported PM2.5 data, by year
# add a linear regression line + the 45 degree line
desc_data %>%
  filter(year >= 2010, year <= 2020) %>%
  filter(country == "China") %>%
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported)) + 
  geom_point(size = .75, color = "grey30") + 
  theme_linedraw() + 
  theme(legend.position = c(.85, .2)) + 
  labs(color = "", y = "Reported PM2.5") + 
  facet_wrap(~year) + 
  xlim(0, 150) + 
  ylim(0, 150) + 
  stat_poly_line(aes(color = "Linear Reg."), se = FALSE) +
  theme(text = element_text(size = 20)) +
  geom_abline(aes(intercept = 0, slope = 1, color = "45 Deg"), size = 0.5, linetype = "dashed", show.legend = FALSE) + 
  scale_color_manual(values = c("black", "blue"))
ggsave(file.path(gdir, "outputs/china_byyear.png"), width = 11, height = 6)  



#==============================================================================
# fraction of countries reporting data by democracy rating
#==============================================================================
country_data <- 
  reg_data %>%
  mutate(has_who_data = !is.na(pm2_5)) %>%
  group_by(countrycode, year, dem) %>%
  summarise(has_who_data = sum(has_who_data) > 0, 
            fiw_median = quantile(fiw_pr, .5, na.rm = TRUE)) %>%
  filter(year > 2009, year < 2022) %>%
  mutate(has_who_data = replace_na(has_who_data, FALSE))
  
country_data %>%
  group_by(dem) %>%
  summarise(has_data = mean(has_who_data)) %>% 
  ggplot(aes(x = dem, y = has_data, fill = dem)) +
  geom_bar(stat = "identity", width = .4) + # Use stat = "identity" for pre-aggregated data
  labs(x = "Democracy Category", y = "Proportion with Data") + 
  theme_linedraw() +
  theme(legend.position = "none", # Remove the legend since 'dem' already labels the x-axis
        text = element_text(size = 20)) + 
  scale_fill_brewer(palette = "Dark2")
ggsave(file.path(gdir, "outputs/hasdata.png"), width = 6, height = 6)  


#==============================================================================
# Graphs visualizing the 2 PM measurements
#==============================================================================
# raw data - binned scatterplot of satellite vs WHO
desc_data %>%
  filter(!china) %>%
  rename(satellite = pm2.5_20km, reported = pm2_5) %>%
  ggplot(aes(x = satellite, y = reported)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .5, color = "#1b9e77") + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  labs(color = "", y = "Reported PM2.5") + 
  stat_poly_line(color = "black") +
  stat_poly_eq(use_label(c("eq", "R2")), size = 8, coef.digits = 2)  + 
  theme(text = element_text(size = 30)) + 
  ylim(0,150) + 
  xlim(0,150)
ggsave(file.path(gdir, "outputs/scatter_raw.png"), width = 11, height = 6)  

# binned scatterplot - just free countries
desc_data %>%
  filter(!china, dem == "Free") %>% 
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reported PM2.5") + 
  stat_poly_line(se = FALSE) +
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150)
ggsave(file.path(gdir, "outputs/scatter_free.png"), width = 11, height = 6)  


# binned scatterplot - free + partially free countries
desc_data %>%
  filter(!china, dem %in% c("Free", "Partially Free")) %>% 
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reported PM2.5") + 
  stat_poly_line(se = FALSE) +
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150)
ggsave(file.path(gdir, "outputs/scatter_partialfree.png"), width = 11, height = 6)  


# binned scatterplot - all data, split by democracy score
desc_data %>%
  filter(!china) %>% 
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reported PM2.5") + 
  stat_poly_line(se = FALSE) +
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150)
ggsave(file.path(gdir, "outputs/scatter_dem.png"), width = 11, height = 6)  



# binned scatterplot - all data, split by democracy score; add China back in
desc_data %>%
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reported PM2.5") + 
  stat_poly_line(se = FALSE) +
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) 
ggsave(file.path(gdir, "outputs/binned_pm_geomsmooth_withchina.png"), width = 11, height = 6)  
