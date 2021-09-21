# Visa waiver dataset
# Analysis

# Data
# year: 2020 scraped in Visa_Scraper.R
# year: 1969 and 2010 retrieved from 
# https://www.fiw.uni-bonn.de/demokratieforschung/personen/laube/visanetworkdata

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "patchwork", "grid", "statnet")

# Load data
### ------------------------------------------------------------------------###
visa.df <- import("./data/VisaNetworkData.RDS")

# Standardized sample 
# i.e. constant Ncountry, reference: 2010
### ------------------------------------------------------------------------ ###
# use country-list of 2010 to standardize 2020
cntry2010 <- colnames(visa.df$data[[2]])

# Standardize
visa.df$data[[3]] <- visa.df$data[[3]][
  rownames(visa.df$data[[3]]) %in% cntry2010, 
  colnames(visa.df$data[[3]]) %in% cntry2010
  ]

# Create in- and outdegree
### ------------------------------------------------------------------------ ###
# Notes: 
# - Check treatment of NA
# - in- and outdegree computation could also be done with the network module
visa.df <- visa.df %>%
  mutate(sent_waivers = map(data, ~rowSums(.x, na.rm = TRUE)),
         received_waivers = map(data, ~colSums(.x, na.rm = TRUE)),
         network_data = map(data, ~network(.x)),
         stats = map(network_data, ~tibble(
           density = network.density(.x), 
           degree = mean(sna::degree(.x, cmode = "indegree"))))) # equal mean outdegree

# Unnest 
### ------------------------------------------------------------------------ ###
visa.stats.df <- visa.df %>%
  select(-data, -network_data) %>%
  mutate(country = map(sent_waivers, ~names(.x))) %>%
  unnest(cols = c(year, sent_waivers, received_waivers, stats, country)) %>%
  nest(data = c(sent_waivers, received_waivers, country))

# Descriptive statistics
### ------------------------------------------------------------------------ ###
visa.stats.df <- visa.stats.df %>%
  mutate(top_10 = map(data, ~.x %>%
                        slice_max(received_waivers, n = 10)), # with_ties 
         bottom_10 = map(data, ~.x %>%
                           slice_min(received_waivers, n = 10))) # with_ties

# Add region identifier (from World Bank Indicators)
### ------------------------------------------------------------------------ ###
visa.stats.df <- visa.stats.df %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        region = countrycode(
                          country, "iso3c", "region",
                          custom_match = c("MKD" = "Europe & Central Asia",
                                           "DDR" = "Europe & Central Asia")))))

# Create variables for plotting
### ------------------------------------------------------------------------ ###
visa.stats.df <- visa.stats.df %>%
  mutate(plot_data = map(data, ~.x %>%
                           mutate(
                             interval = cut(received_waivers, 
                                            breaks = c(-Inf, seq(1, 106, 5)), 
                                            right = FALSE,
                                            labels = 
                                              c("0", paste0(seq(1, 101, 5), "-", 
                                                            seq(5, 106, 5))))) %>%
           group_by(interval) %>%
           count() %>%
           ungroup() %>%
           mutate(cum_sum = cumsum(n)/sum(n))
           ))

# Plot
### ------------------------------------------------------------------------ ###
visa.stats.df <- visa.stats.df %>%
  mutate(barplot = 
           map2(.x = plot_data, .y = year, ~ggplot(.x) +
                 geom_bar(aes(x = interval, y = n), stat = "identity") +
                 scale_y_continuous(limits = c(0, 40)) +
                 scale_x_discrete(drop = FALSE) +
                 labs(x = "", y = "", title = paste(.y)) +
                 theme_minimal() +
                 theme(
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1)
                 )),
         lineplot = 
           map2(.x = plot_data, .y = year, ~ggplot(.x) +
                 geom_line(aes(x = interval, y = cum_sum, group = 1)) +
                 geom_area(aes(x = interval, y = cum_sum, group = 1), 
                           fill = "#252525", alpha = 0.4) +
                 scale_x_discrete(drop = FALSE) +
                 scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
                 labs(x = "", y = "", title = paste(.y)) +
                 theme_minimal() +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1))
                 ))

# Use patchwork to arrange plots
### ------------------------------------------------------------------------ ###
plot.df <- visa.stats.df %>%
  select("barplot", "lineplot", "year") %>%
  pivot_longer(c("barplot", "lineplot"), names_to = "type", values_to = "plot") %>%
  arrange(type, year) %>%
  pull(plot) 

# Wrap plots
visa.histogram.fig <- wrap_plots(plot.df, ncol = 3, nrow = 2, heights = c(3, 1))

# Export
### ------------------------------------------------------------------------ ###
ggsave(
  plot = visa.histogram.fig, "./figures/histogram-visa.tiff", 
  width = 16, height = 10, unit = "in",
  dpi = 300
)
