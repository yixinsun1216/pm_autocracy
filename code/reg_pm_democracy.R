# ==========================================================================
# Purpose: Run analysis testing for autocracy gradient in PM2.5 reporting
# Last updated: 10/11/2023
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

# Directories
rdir <- file.path(ddir, "data")


#==============================================================================
# Read in data
#==============================================================================
reg_data <- readRDS(file.path(ddir, "generated_data", "reg_data.RDS")) %>%
  mutate(pm_diff = pm2_5 - pm2.5_20km, 
         china = country == "CHINA", 
         country = countrycode(countrycode, "iso3c", "country.name"), 
         subregion = countrycode(countrycode, "iso3c", "un.regionsub.name")) 


satellite_measure <- "pm2.5_20km"

outdict <- 
  c("pm2_5" = "WHO Reported PM2.5", 
    "pm2.5_10km" = "Satellite PM2.5", 
    "pm2.5_20km" = "Satellite PM2.5", 
    "pm2.5_50km" = "Satellite PM2.5", 
    "demPartially Free" = "I(Partially Free)",
    "demNot Free" = "I(Not Free)",
    fiw_pr = "FiW", 
    year = "Year", 
    countrycode = "Country", 
    subregion = "UN Sub-Region",
    has_who_data = "I(Reported Data to WHO)", 
    polity2_neg = "Autocracy measure", 
    democ_neg = "Autocracy measure", 
    autoc = "Autocracy measure")

#==============================================================================
# Can I predict who has WHO data based on autocracy gradient?
#==============================================================================
country_data <- 
  reg_data %>%
  mutate(has_who_data = !is.na(pm2_5)) %>%
  group_by(countrycode, year, dem, fiw_pr, subregion) %>%
  summarise(has_who_data = sum(has_who_data) > 0) %>%
  filter(year > 2009, year < 2022) %>%
  mutate(has_who_data = replace_na(has_who_data, FALSE))

f_reg_ext <-
  c(paste0("has_who_data ~ dem |subregion + year"), 
    paste0("has_who_data ~ fiw_pr |subregion + year")) 

reg_ext <- map(f_reg_ext, ~feols(as.formula(.x), data = country_data, cluster = ~countrycode)) 
etable(reg_ext, dict = outdict, tex = TRUE, replace = TRUE, fitstat = c("n", "ar2"),
       file = file.path(gdir, "outputs/reg_extensive_margin.tex"))


#==============================================================================
# run regressions of difference in PM on FIW
#==============================================================================
f_reg <-
  c(paste0("pm2_5 ~ ", satellite_measure, "*dem |subregion + year"), 
    paste0("pm2_5 ~ ", satellite_measure, "*fiw_pr |subregion + year")) 


reg_nochina <- map(f_reg, ~feols(as.formula(.x), data = filter(reg_data, countrycode != "CHN"), cluster = ~countrycode)) 

etable(reg_nochina, dict = c(outdict, "chinaTRUE" = "China"), 
       tex = TRUE, replace = TRUE,
       file = file.path(gdir, "outputs/reg_main.tex"))

# with China
reg_all <- map(f_reg, ~feols(as.formula(.x), data = reg_data, cluster = ~countrycode)) 
etable(reg_all, dict = c(outdict, "chinaTRUE" = "China"), 
       tex = TRUE, replace = TRUE,
       file = file.path(gdir, "outputs/reg_main_withchina.tex"))


#==============================================================================
# Robustness
#==============================================================================
f_reg_robust <-
  c(paste0("pm2_5 ~ pm2.5_20km*dem |subregion + year"),
    paste0("pm2_5 ~ pm2.5_10km*dem |subregion + year"), 
    paste0("pm2_5 ~ pm2.5_50km*dem |subregion + year"),
    paste0("pm2_5 ~ pm2.5_20km*dem |countrycode"),
    paste0("pm2_5 ~ pm2.5_20km*dem |year"),
    paste0("pm2_5 ~ pm2.5_20km*dem |countrycode + year")) 

reg_dist <- map(f_reg_robust, ~feols(as.formula(.x), data = reg_data, cluster = ~countrycode)) 
reg_dist_nochina <- map(f_reg_robust, ~feols(as.formula(.x), data = filter(reg_data, countrycode != "CHN"), cluster = ~countrycode)) 


etable(reg_dist, dict = outdict, 
       headers = c("Main Spec", "10km", "50km", "Country FE", "Year", "Country + Year FE"), 
       depvar = FALSE, tex = TRUE, replace = TRUE, 
       file = file.path(gdir, "outputs/reg_robust.tex"))

etable(reg_dist_nochina, dict = outdict, 
       headers = c("Main Spec", "10km", "50km", "Country FE", "Year", "Country + Year FE"), 
       depvar = FALSE, tex = TRUE, replace = TRUE, 
       file = file.path(gdir, "outputs/reg_robust_nochina.tex"))


f_reg_fiw_robust <-
  c(paste0("pm2_5 ~ pm2.5_20km*fiw_pr |subregion + year"),
    paste0("pm2_5 ~ pm2.5_10km*fiw_pr |subregion + year"), 
    paste0("pm2_5 ~ pm2.5_50km*fiw_pr |subregion + year"),
    paste0("pm2_5 ~ pm2.5_20km*fiw_pr + fiw_pr^2|subregion + year"),
    paste0("pm2_5 ~ pm2.5_20km*fiw_pr |countrycode"),
    paste0("pm2_5 ~ pm2.5_20km*fiw_pr |year"),
    paste0("pm2_5 ~ pm2.5_20km*fiw_pr |countrycode + year")) 

reg_fiw_dist <- map(f_reg_fiw_robust, ~feols(as.formula(.x), data = reg_data, cluster = ~countrycode)) 

etable(reg_fiw_dist, dict = outdict, 
       headers = c("Main Spec", "10km", "50km", "FiW Quad", "Country FE", "Year", "Country + Year FE"), 
       depvar = FALSE, tex = TRUE, replace = TRUE, 
       file = file.path(gdir, "outputs/reg_fiw_robust.tex"))

reg_fiw_dist_nochina <- map(f_reg_fiw_robust, ~feols(as.formula(.x), data = filter(reg_data, countrycode != "CHN"), cluster = ~countrycode)) 
etable(reg_fiw_dist_nochina, dict = outdict, 
       headers = c("Main Spec", "10km", "50km", "FiW Quad", "Country FE", "Year", "Country + Year FE"), 
       depvar = FALSE, tex = TRUE, replace = TRUE, 
       file = file.path(gdir, "outputs/reg_fiw_robust_nochina.tex"))



#==============================================================================
# robustenss - exclude one country at a time
#==============================================================================
country_list <- unique(reg_data$countrycode)

# run regression one at a time,where each regression excludes one country
coefs_exclude_country <- 
  map_df(country_list[-which(country_list == "CHN")], function(x){
    data <- filter(reg_data, countrycode != x, !china)
    
    feols(pm2_5 ~ pm2.5_20km*fiw_pr |subregion + year, data) %>%
      broom::tidy(conf.int = TRUE) %>%
      filter(term == "pm2.5_20km:fiw_pr") %>%
      mutate(country := !!x) }) %>%
  arrange(estimate) %>%
  mutate(Regression = row_number())

# same as above, but this time we include china
coefs_include_china <- map_df(unique(reg_data$countrycode), function(x){
  data <- filter(reg_data, countrycode != x)
  feols(pm2_5 ~ pm2.5_20km*fiw_pr |subregion + year, data) %>%
    broom::tidy(conf.int = TRUE) %>%
    filter(term == "pm2.5_20km:fiw_pr") %>%
    mutate(country := !!x)
  }) %>%
  arrange(estimate) %>%
  mutate(Regression = row_number())

# combine the two types of robustness checks into one figure
coefs_exclude_country %>%
  mutate(exclude = "Exclude China") %>%
  bind_rows(coefs_include_china) %>%
  mutate(exclude = replace_na(exclude, "Include China"), 
         china = if_else(country == "CHN", "China", "")) %>%
  ggplot(aes(x = Regression, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "grey50",
                position = position_dodge2(width = 0.5, padding = 0.5)) + 
  theme_linedraw() +
  facet_wrap(~exclude) + 
  theme(text = element_text(size = 20)) + 
  geom_text(aes(label = china), hjust = -.2, color = "maroon")
ggsave(file.path(gdir, "outputs/robust_exclude_country.png"), width = 13, height = 8)


#==============================================================================
# robustenss - exclude one subregion at a time
#==============================================================================
# same as above, but this time use subregions as defined by the UN. There are 22 
# subregions in the world
reg_data <- filter(reg_data, !is.na(subregion))

coefs_exclude_subregion <- 
  unique(reg_data$subregion) %>%
  map_df(function(x){
    data <- filter(reg_data, subregion != x, countrycode != "CHN")
    
    feols(pm2_5 ~ pm2.5_20km*fiw_pr |subregion + year, data) %>%
      broom::tidy(conf.int = TRUE) %>%
      filter(term == "pm2.5_20km:fiw_pr") %>%
      mutate(Subregion := !!x) 
    }) %>%
  arrange(estimate) %>%
  mutate(Regression = row_number())

coefs_subregion_include_china <- 
  unique(reg_data$subregion) %>%
  map_df(function(x){
    
    data <- filter(reg_data, subregion != x)
    
    feols(pm2_5 ~ pm2.5_20km*fiw_pr |subregion + year, data) %>%
      broom::tidy(conf.int = TRUE) %>%
      filter(term == "pm2.5_20km:fiw_pr") %>%
      mutate(Subregion := !!x)
    }) %>%
  arrange(estimate) %>%
  mutate(Regression = row_number())

coefs_exclude_subregion %>%
  mutate(exclude = "Exclude China") %>%
  bind_rows(coefs_subregion_include_china) %>%
  mutate(exclude = replace_na(exclude, "Include China")) %>%
  ggplot(aes(x = Subregion, y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "grey50",
                position = position_dodge2(width = 0.5, padding = 0.5), width = .3) + 
  theme_linedraw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
        plot.margin = unit(c(0, 0, 0, 1), "inches")) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") + 
  facet_wrap(~exclude)
ggsave(file.path(gdir, "outputs/robust_exclude_subregion.png"), width = 13, height = 7)


