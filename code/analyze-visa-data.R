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
visa.df <- import("./data/VisaNetworkData.RDS")

# Standardized sample 
# i.e. constant Ncountry, reference: 2010
### ------------------------------------------------------------------------ ###
# use country-list of 2010 to standardize 2020
cntry2010 <- colnames(visa.df$data[[2]])

# Standardize
visa.df$data[[3]] <- visa.df$data[[3]][
  rownames(visa.df$data[[3]]) %in% cntry2010, 
  colnames(visa.df$data[[3]]) %in% cntry2010]

# Create in- and outdegree
### ------------------------------------------------------------------------ ###
visa.df <- visa.df %>%
  mutate(sent_waivers = map(data, ~rowSums(.x, na.rm = TRUE)),
         received_waivers = map(data, ~colSums(.x, na.rm = TRUE)),
         network_data = map(data, ~network(.x)),
         stats = map(network_data, ~tibble(
           density = network.density(.x), 
           degree = mean(sna::degree(.x, cmode = "indegree"))))) # equal mean outdegree

# Unnest 
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa.df %>%
  select(-data, -network_data) %>%
  mutate(country = map(sent_waivers, ~names(.x))) %>%
  unnest(cols = c(year, sent_waivers, received_waivers, stats, country)) %>%
  nest(data = c(sent_waivers, received_waivers, country))

# Descriptive statistics
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa_stats.df %>%
  mutate(top_10 = map(data, ~.x %>%
                        slice_max(received_waivers, n = 10)), # with_ties 
         bottom_10 = map(data, ~.x %>%
                           slice_min(received_waivers, n = 10))) # with_ties

# Add region identifier (from World Bank Indicators)
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa_stats.df %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        region = countrycode(
                          country, "iso3c", "region",
                          custom_match = c("MKD" = "Europe & Central Asia",
                                           "DDR" = "Europe & Central Asia")))))

# Variables grouped by region
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa_stats.df %>%
  mutate(data = map(data, ~.x %>%
                      group_by(region) %>%
                      mutate(region_mean_sent = mean(sent_waivers, 
                                                     na.rm = TRUE),
                             region_mean_received = mean(received_waivers, 
                                                         na.rm = TRUE)) %>%
                      ungroup() %>%
                      mutate(global_mean_sent = mean(sent_waivers, 
                                                     na.rm = TRUE),
                             global_mean_received = mean(received_waivers, 
                                                         na.rm = TRUE))))

# Countries that lost/won most visa waivers
### ------------------------------------------------------------------------ ###
# Create df
visa_long.df <- visa.df %>%
  select(year, received_waivers, sent_waivers) %>%
  mutate(
    received_waivers = map(received_waivers, ~enframe(
      .x,
      name = "country", 
      value = "received_waivers")),
    sent_waivers = map(sent_waivers, ~enframe(
      .x,
      name = "country", 
      value = "sent_waivers")),
    visa = map2(.x = received_waivers, .y = sent_waivers, ~left_join(
      x = .x, 
      y = .y, 
      by = "country"))) %>%
  select(-c("sent_waivers", "received_waivers")) %>%
  unnest(c(visa)) %>%
  pivot_wider(id_cols = c(country, year), names_from = year, 
              values_from = c(received_waivers, sent_waivers)) 

# Most won/lost 1969:2010
visa_long.df %>%
  mutate(diff = received_waivers_1969 - received_waivers_2010) %>%
  filter(!is.na(diff)) %>%
  arrange(desc(diff)) %>%
  slice(1:5, n():(n()-5)) %>%
  select(country, received_waivers_1969, received_waivers_2010, 
         received_waivers_2020) %>%
  mutate(position = factor(ifelse(row_number() == 1:5, "Winner", "Loser"), levels = c("Winner", "Loser"))) %>%
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "number") %>%
  mutate(year = str_extract(variable, "[:digit:]{4}"),
         variable = str_replace(variable, "_[:digit:]{4}", "")) %>%
  filter(variable == "received_waivers") %>%
  ggplot(aes(x = year, y = number, colour = country, group = country)) +
  geom_point() +
  geom_path() +
  facet_wrap(~position, ncol = 1) +
  theme_basic

# Create variables for plotting
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa_stats.df %>%
  mutate(plot_data = map(data, ~.x %>%
                           mutate(
                             interval = cut(received_waivers, 
                                            breaks = c(-Inf, seq(1, 106, 5)), 
                                            right = FALSE,
                                            labels = 
                                              c("0", paste0(seq(1, 101, 5), "-", 
                                                            seq(5, 106, 5)))))),
         mode = map_chr(plot_data, ~modeest::mfv(as.character(.x$interval))),
         plot_data = map(plot_data, ~.x %>%
                           group_by(interval) %>%
                           count() %>%
                           ungroup() %>%
                           mutate(cum_sum = cumsum(n)/sum(n))))

# Plot histogram and cumulative frequency distribution across years
### ------------------------------------------------------------------------ ###
visa_stats.df <- visa_stats.df %>%
  mutate(
    # Individual annotations
    x_axis = c("", "Visa-free travel", ""),
    y_axis_bar = c("Frequency", "", ""),
    y_axis_line = c("Percentage", "", ""),
    y_axis_point = c("Region", "", ""),
    
    # Barplot of degree distribution
    barplot =
      pmap(list(plot_data, y_axis_bar, year), ~ ggplot(..1) +
        geom_bar(aes(x = interval, y = n), stat = "identity") +
        scale_y_continuous(limits = c(0, 40)) +
        scale_x_discrete(drop = FALSE) +
        labs(x = "", y = ..2, title = str_c("Histogram: Visa-free travel, ", ..3)) +
        theme_basic +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14))),
    
    # Cumulative frequency distribution of degree
    lineplot =
      pmap(list(plot_data, x_axis, y_axis_line, year), ~ ggplot(..1) +
        geom_line(aes(x = interval, y = cum_sum, group = 1)) +
        geom_area(aes(x = interval, y = cum_sum, group = 1),
          fill = "#252525", alpha = 0.4) +
        scale_x_discrete(drop = FALSE) +
        scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
        labs(x = ..2, y = ..3, title = str_c("Cumulative frequency, ", ..4)) +
        theme_basic +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14))),
    
    # Pointplot of received visas by regions
    received_pointplot = pmap(list(data, x_axis, y_axis_point, year), ~ggplot(..1, aes(
      x = factor(region, levels = rev(c(
        "North America", "Europe & Central Asia", "Latin America & Caribbean",
        "East Asia & Pacific", "Middle East & North Africa", "Sub-Saharan Africa",
        "South Asia"))),
      y = received_waivers)) +
      geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
      geom_point(data = ..1 %>% 
                   distinct(region, region_mean_received), 
                 aes(x = region, y = region_mean_received),
        size = 5, shape = 15, alpha = 0.75) +
      geom_hline(aes(yintercept = global_mean_received),
        color = "black",
        size = 0.6, 
        linetype = "dashed") +
      coord_flip() +
      ylim(0, 105) +
      labs(x = ..3, y = ..2, title = str_c("Visa-free travel, ", ..4)) +
      theme_basic)) %>%
  select(-c(x_axis, y_axis_bar, y_axis_line, y_axis_point))

# Use patchwork to arrange plots
### ------------------------------------------------------------------------ ###
# (A) Histogram and cumulative frequency
hist_plot.df <- visa_stats.df %>%
  select("barplot", "lineplot", "year") %>%
  pivot_longer(c("barplot", "lineplot"), names_to = "type", values_to = "plot") %>%
  arrange(type, year) %>%
  pull(plot) 

# Wrap plots
visa_histogram.fig <- wrap_plots(hist_plot.df, ncol = 3, nrow = 2, heights = c(3, 1))

# (B) Visa freedom by region
region_plot.df <- visa_stats.df %>%
  pull(received_pointplot)

# Wrap plots
region.fig <- wrap_plots(region_plot.df, nrow = 1)

# Export
### ------------------------------------------------------------------------ ###
ggsave(
  plot = visa_histogram.fig, "./figures/histogram-visa.tiff", 
  width = 18, height = 10, unit = "in",
  dpi = 300
)

ggsave(
  plot = region.fig, "./figures/region-visa.tiff", 
  width = 18, height = 10, unit = "in",
  dpi = 300
)
