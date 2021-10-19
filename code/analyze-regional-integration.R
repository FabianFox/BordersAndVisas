# Analyze regional integration of visa waivers

# Data
# year: 2020 scraped in Visa_Scraper.R
# year: 1969 and 2010 retrieved from 
# https://www.fiw.uni-bonn.de/demokratieforschung/personen/laube/visanetworkdata

# on regional integration
# from: https://riks.cris.unu.edu/ scraped in scrape-regional-integration-membership.R

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "grid", "statnet")

# Theme (plotting)
### ------------------------------------------------------------------------ ###
theme_basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text = element_text(colour = "black", size = 14)
  )

# Load data
### ------------------------------------------------------------------------ ###
# Visa data
visa.df <- import("./data/VisaNetworkData.RDS")

# Data on egional integration membership
integration.df <- import("./data/regional_integration.RDS")

# Standardized sample 
# i.e. constant Ncountry, reference: 2010
### ------------------------------------------------------------------------ ###
# use country-list of 2010 to standardize 2020
cntry2010 <- colnames(visa.df$data[[2]])

# Standardize
visa.df$data[[3]] <- visa.df$data[[3]][
  rownames(visa.df$data[[3]]) %in% cntry2010, 
  colnames(visa.df$data[[3]]) %in% cntry2010]

# Compute external openness as percentage of visa waivers given the total number 
# of states (mean average for regional integration entities)
### ------------------------------------------------------------------------ ###
# External openness function
external_openness_fun <- function(x){
  reshape2::melt(visa.df$data[[3]], 
                 varnames = c("from", "to")) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(from != to) %>%
    filter(!to %in% integration.df$country[integration.df$project_name == x]) %>%
    mutate(n = length(unique(to))) %>%
    group_by(from) %>%
    summarise(external_openness = sum(value) / n * 100) %>%
    ungroup() %>%
    distinct(from, external_openness) %>%
    filter(from %in% integration.df$country[integration.df$project_name == x]) %>%
    summarise(mean = mean(external_openness))
  }

# Internal openness
internal_openness_fun <- function(x){
  reshape2::melt(visa.df$data[[3]], 
                 varnames = c("from", "to")) %>%
    mutate(across(where(is.factor), as.character)) %>%
    filter(from != to) %>%
    filter(across(c(from, to), ~ .x %in% integration.df$country[integration.df$project_name == x])) %>%
    mutate(n = length(unique(to)) - 1) %>%
    group_by(from) %>%
    mutate(internal_openness = sum(value) / n * 100) %>%
    ungroup() %>%
    distinct(from, internal_openness) %>%
    filter(from %in% integration.df$country[integration.df$project_name == x]) %>%
    summarise(mean = mean(internal_openness))
  }

# Apply the function
integration_stats.df <- integration.df %>%
  select(project_name) %>%
  distinct() %>%
  mutate(external_openness = map_dbl(project_name, ~unlist(external_openness_fun(.x))),
         internal_openness = map_dbl(project_name, ~unlist(internal_openness_fun(.x))))

# Make data longer for plotting
integration_plot.df <- integration_stats.df %>%
  pivot_longer(cols = c(external_openness, internal_openness), names_to = "measure") %>%
  arrange(desc(measure), value) %>%
  mutate(project_name = factor(project_name, levels = unique(project_name)),
         measure = str_to_title(str_replace(measure, "_", " ")))

# Plot
### ------------------------------------------------------------------------ ###
integration_openness.fig <- ggplot(integration_plot.df, aes(x = project_name, 
                                                            value, y = value,
                                                            fill = measure)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(labels = function(x) str_c(x, "%")) + 
  labs(x = "", y = "Percentage", fill = "") +
  theme_basic +
  theme(legend.text = element_text(size = 14))

# Export
### ------------------------------------------------------------------------ ###
ggsave(plot = integration_openness.fig, filename = "./figures/regional-integration-openness.tiff")
