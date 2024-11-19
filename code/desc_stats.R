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
pacman::p_load(tidyverse, countrycode, ggplot2, fixest, ggpmisc, marginaleffects)

# get the path to the active R file
working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# use this path to laod file paths that will be used throughout the document
source(file.path(working_dir, "filepaths.R"))

# theme that i will continuously use
theme_bias <- function(){
  theme(
    legend.position = "none",
    axis.text = element_text(size = 18), 
    title = element_text(size = 14),
    panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
    plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
    legend.background = element_rect(fill = "transparent", color = NA),  # Legend background,
    panel.border = element_blank(),       # Remove panel border
    panel.grid = element_blank(),         # Remove grid lines
    axis.line.x = element_blank(),        # Remove x-axis line
    axis.line.y = element_line(color = "gray70"),         # Keep y-axis line
    axis.ticks.x = element_blank(),       # Remove x-axis ticks
    axis.ticks.y = element_line(color = "gray70"),        # Keep y-axis ticks
    axis.title.x = element_blank()        # Optionally remove x-axis title
  )
}
  

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
  select(Satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = Satellite, y = reported)) + 
  geom_point(size = .75, color = "grey30") + 
  theme_linedraw() + 
  theme(legend.position = c(.85, .2)) + 
  labs(color = "", y = "Government Reported PM2.5") + 
  facet_wrap(~year) + 
  xlim(0, 150) + 
  ylim(0, 150) + 
  stat_poly_line(
    data = . %>% filter(!(year %in% c(2011, 2012))),
    aes(color = "Linear Reg."),
    se = FALSE
  ) +
  theme(text = element_text(size = 20), 
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
        legend.background = element_rect(fill = "transparent", color = NA))+  # Legend background,) +
  geom_abline(aes(intercept = 0, slope = 1, color = "45 Deg"), size = 0.5, linetype = "dashed", show.legend = FALSE) + 
  scale_color_manual(values = c("black", "blue"))
ggsave(file.path(gdir, "outputs/china_byyear.png"), width = 11, height = 6)  

stop()
# run linear regression of pm2.5 vs satellite - take the coefficient
# and subtract it from 1 to calculating a reporting bias
reg_china <- 
  feols(pm2_5 ~ pm2.5_20km, data = desc_data %>% filter(country == "China"), split = ~year, se = "hetero") %>%
  map_df(function(x){
    # hypothesis test for whether the coefficient equals 1 to measure bias
    marginaleffects::hypotheses(x, hypothesis = "pm2.5_20km = 1") %>%
      select(estimate, std.error) %>%
      tibble() %>% 
      mutate(year = as.numeric(x$model_info$sample$value), 
             n = x$nobs)
  }) %>%
  mutate(bias = if_else(n > 10, estimate, NA_real_), 
         prewar = year < 2013) 

# plot the bias for each year
reg_china %>%
  ggplot(aes(x = year, y = bias, color = prewar)) + 
  geom_point(size =4) +
  geom_errorbar(aes(ymin = bias - 1.96*std.error, ymax = bias + 1.96*std.error), width = .2) +
  theme_minimal() + 
  theme_bias() +
  geom_hline(aes(yintercept = 0), color = "gray70") + 
  ylim(-.8, .8) + 
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +  # Ensure all years are shown
  geom_vline(xintercept = 2013, linetype = "dashed", color = "#984ea3") +
  geom_text(aes(x = 2012, y = .5, label = "Beijing's Airpocalypse \nHits Front Page"), 
            color = "#984ea3", size = 4) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "#2166ac") +
  geom_text(aes(x = 2014.6, y = .4, label = "China's War \non Pollution"), 
            color = "#2166ac", size = 4) + 
  ylab("Reporting Bias") + 
  scale_color_manual(values = c("black", "darkred")) + 
  annotate("rect", xmin = 2010.5, xmax = 2012.5, ymin = -Inf, # Cover the full range of y-axis
    ymax = Inf, fill = "gray70", alpha = 0.2) + 
  geom_text(aes(x = 2011.5, y = -.05, label = "No Data"), 
            color = "gray50", size = 4) +
  ggtitle("China's Underreporting Bias Over Time")
ggsave(file.path(gdir, "outputs/china_reportingbias.png"), width = 11, height = 6)  




#==============================================================================
# Graphs visualizing the 2 PM measurements
#==============================================================================
# raw data - binned scatterplot of satellite vs WHO
desc_data %>%
  filter(!china) %>%
  rename(satellite = pm2.5_20km, reported = pm2_5) %>%
  ggplot(aes(x = satellite, y = reported)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .5, color = "#1b9e77") + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  labs(color = "", y = "PM2.5 Reported to WHO") + 
  stat_poly_line(color = "black") +
  stat_poly_eq(use_label(c("eq", "R2")), size = 8, coef.digits = 2)  + 
  theme(text = element_text(size = 30)) + 
  ylim(0,150) + 
  xlim(0,150)
ggsave(file.path(gdir, "outputs/scatter_raw.png"), width = 11, height = 6)  

# run regression to calculate the bias by autocracy score
reg_bias_dem <- 
  feols(pm2_5 ~ pm2.5_20km, data = filter(desc_data, !china), split = ~dem) %>%
  map_df(function(x){
    # hypothesis test for whether the coefficient equals 1 to measure bias
    marginaleffects::hypotheses(x, hypothesis = "pm2.5_20km = 1") %>%
      select(estimate, std.error) %>%
      tibble() %>% 
      mutate(dem = x$model_info$sample$value, 
             n = x$nobs)
  }) %>%
  mutate(bias = estimate, 
         dem = str_wrap(dem, width = 10),
         dem = factor(dem, levels = c("Free", "Partially\nFree", "Not Free")))

# binned scatterplot - just free countries -------------------------------
p_scatter_free <- desc_data %>%
  filter(!china, dem == "Free") %>% 
  select(Satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = Satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reportd to WHO") + 
  stat_poly_line(se = FALSE) +
  theme(axis.text = element_text(size = 18), legend.position = "bottom", 
        #axis.title.y = element_blank(),
        title = element_text(size = 14),
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
        legend.background = element_rect(fill = "transparent", color = NA)) +   # Legend background 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150) +
  ggtitle(expression("PM"[2.5]* " (measured in " ~ µg/m^3 ~ ")"))

p_bias_free <- 
  reg_bias_dem%>%
  mutate(bias = if_else(dem == "Free", bias, NA_real_)) %>%
  ggplot(aes(x = dem, y = bias, color = dem)) + 
  geom_point(size = 3) +
  ylim(-.4, .4) + 
  geom_errorbar(aes(ymin = bias - 1.96*std.error, ymax = bias + 1.96*std.error), width = .2) +
  theme_minimal() + 
  theme_bias() +
  geom_hline(aes(yintercept = 0), color = "gray70") + 
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  ggtitle("Underreporting Bias")

p_scatter_free + plot_spacer() + p_bias_free + plot_layout(widths = c(1.5, .1, 1)) &
  theme(plot.background = element_rect(fill='transparent'),
        legend.background = element_rect(fill = 'transparent'))
ggsave(file.path(gdir, "outputs/scatter_free.png"), width = 11, height = 6, bg = "transparent")  


# binned scatterplot - free + partially free countries ----------------------
p_scatter_partial <- 
  desc_data %>%
  filter(!china, dem %in% c("Free", "Partially Free")) %>% 
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reportd to WHO") + 
  stat_poly_line(se = FALSE) +
  theme(axis.text = element_text(size = 18), legend.position = "bottom", 
        title = element_text(size = 14),
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
        legend.background = element_rect(fill = "transparent", color = NA)) +   # Legend background 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150) +
  ggtitle(expression("PM"[2.5]* " (measured in " ~ µg/m^3 ~ ")"))


p_bias_partial <- 
  reg_bias_dem%>%
  mutate(bias = if_else(dem %in% c("Free", "Partially\nFree"), bias, NA_real_)) %>%
  ggplot(aes(x = dem, y = bias, color = dem)) + 
  geom_point(size = 3) +
  ylim(-.4, .4) + 
  ylab("Reporting Bias")+ 
  geom_errorbar(aes(ymin = bias - 1.96*std.error, ymax = bias + 1.96*std.error), width = .2) +
  theme_minimal() + 
  theme_bias() +
  geom_hline(aes(yintercept = 0), color = "gray70") + 
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  ggtitle("Underreporting Bias")

(p_scatter_partial + plot_spacer() + p_bias_partial)  + 
  plot_layout(widths = c(1.5, .1, 1)) &
  theme(plot.background = element_rect(fill='transparent'),
        legend.background = element_rect(fill = 'transparent'))

ggsave(file.path(gdir, "outputs/scatter_partialfree.png"), width = 11, height = 6, bg = "transparent")  

# binned scatterplot - all data, split by democracy score ----------------------
p_scatter_dem <- 
  desc_data %>%
  filter(!china) %>% 
  select(satellite = pm2.5_20km, reported = pm2_5, year, city, country, dem) %>%
  ggplot(aes(x = satellite, y = reported, color = dem)) + 
  stat_summary_bin(fun = "mean", bins = 100, size = .2) + 
  theme_classic() + 
  scale_color_brewer(palette = "Dark2") +
  labs(color = "", y = "Reportd to WHO") + 
  stat_poly_line(se = FALSE) +
  theme(axis.text = element_text(size = 18), legend.position = "bottom", 
        #axis.title.y = element_blank(),
        title = element_text(size = 14),
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
        legend.background = element_rect(fill = "transparent", color = NA)) +   # Legend background 
  stat_poly_eq(use_label(c("eq", "R2")), size = 5, coef.digits = 2) +
  xlim(0, 150) +
  ggtitle(expression("PM"[2.5]* " (measured in " ~ µg/m^3 ~ ")"))



p_bias_dem <- 
  reg_bias_dem%>%
  ggplot(aes(x = dem, y = bias, color = dem)) + 
  geom_point(size = 3) +
  ylim(-.4, .4) + 
  geom_errorbar(aes(ymin = bias - 1.96*std.error, ymax = bias + 1.96*std.error), width = .2) +
  theme_minimal() + 
  theme_bias() +
  geom_hline(aes(yintercept = 0), color = "gray70") + 
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  ggtitle("Underreporting Bias")

p_scatter_dem + plot_spacer() + p_bias_dem + plot_layout(widths = c(1.5, .1, 1))&
  theme(plot.background = element_rect(fill='transparent'),
        legend.background = element_rect(fill = 'transparent'))

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
ggsave(file.path(gdir, "outputs/scatter_withchina.png"), width = 11, height = 6)  


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
  
p_everreport <- country_data %>%
  group_by(dem) %>%
  summarise(has_data = mean(has_who_data)) %>% 
  mutate(dem = str_wrap(dem, width = 10),
         dem = factor(dem, levels = c("Free", "Partially\nFree", "Not Free"))) %>%
  ggplot(aes(x = dem, y = has_data, fill = dem)) +
  geom_bar(stat = "identity", width = .4) + # Use stat = "identity" for pre-aggregated data
  # labs(y = "Proportion Reporting Data") + 
  theme_classic() +
  theme(legend.position = "none", # Remove the legend since 'dem' already labels the x-axis
        axis.text = element_text(size = 18), 
        title = element_text(size = 14), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel background
        plot.background = element_rect(fill = "transparent", color = NA),   # Plot background
        legend.background = element_rect(fill = "transparent", color = NA)) +   # Legend background  
  scale_fill_brewer(palette = "Dark2") + 
  scale_y_continuous(expand = c(0, 0))  +
  ggtitle("Proportion of Countries Reporting WHO Data")

p_bias_dem + plot_spacer() +  p_everreport + plot_layout(widths = c(1, .1, 1)) &
  theme(plot.background = element_rect(fill='transparent'),
        legend.background = element_rect(fill = 'transparent'))
ggsave(file.path(gdir, "outputs/bias_report.png"), width = 12, height = 6)  

