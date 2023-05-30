# Number of border fences over time

# Data: Avdan (2019, p. 128-129)
# Source:
# Avdan, Nazli (2019): Visas and Walls: Border Security in the Age of Terrorism,
# Philadelphia: University of Pennsylvania Press.

# Notes
# - excluding dismantled and unfinished border fences
# - n = 56

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "Cairo")

# Plot theme
# Additional settings (i.e. for figures)
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text = element_text(colour = "black", size = 14)
  )

# Load data
# The original tables have been extracted from the pdf-version of the book
### ------------------------------------------------------------------------ ###
# Page 128
barriers.av.t1 <- import("./data/Avdan 2019 - Visas and Walls_1.xlsx", 
                         range = "A3:D44") %>%
  .[-36,] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         indicator = "fortified",
         source = "Avdan (2019)")

# Page 129
barriers.av.t2 <- import("./data/Avdan 2019 - Visas and Walls_2.xlsx", 
                         range = "A2:D41") %>%
  .[-c(27, 35),] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         indicator = "fortified",
         source = "Avdan (2019)")

# (2) Combine
barriers.df <- bind_rows(barriers.av.t1, barriers.av.t2)

# Some countrycodes are not matched properly
barriers.df[10,"state1"] <- "GDR"
barriers.df[c(11, 12), "state1"] <- "CSK"
barriers.df[15, c("state1", "state2")] <- c("KOR", "PRK")
barriers.df[27, "state2"] <- "PRK"
barriers.df[45, "state2"] <- "PRK"

# Remove "intended fences"
## -------------------------------------------------------------------------- ##
barriers.df <- barriers.df %>%
  filter(!is.na(year)) %>%
  select(-c(indicator, source))

# Create variables for plotting
## -------------------------------------------------------------------------- ##
# Built fences
fence.df <- barriers.df %>%
  mutate(period_five = cut(year, breaks = seq(1901, 2016, 5), right = FALSE,
                           labels = paste0(seq(1901, 2011, 5), "-", 
                                           seq(1905, 2015, 5)))) %>%
  count(year = period_five) %>%
  mutate(n_cumsum = cumsum(n))

# Dismantled fences
dismantled_fence.df <- barriers.df %>%
  filter(!is.na(dismantled)) %>%
  mutate(period_five = cut(dismantled, breaks = seq(1901, 2016, 5), right = FALSE,
                           labels = paste0(seq(1901, 2011, 5), "-", 
                                           seq(1905, 2015, 5)))) %>%
  count(year = period_five) %>%
  mutate(n_cumsum = cumsum(n))
  
# Join built & dismantled fences
fence.df <- fence.df %>%
  left_join(y = dismantled_fence.df %>%
              rename(n_dismantled = n, n_cumsum_dismantled = n_cumsum), by = "year") %>%
  fill(n_cumsum_dismantled)

# No dismantled fences in first observation period
fence.df[1, "n_cumsum_dismantled"] <- 0

# Cumulative sum of built and dismantled barriers
fence.df <- fence.df %>%
  mutate(n_cumsum = n_cumsum - n_cumsum_dismantled)

# Plot
## -------------------------------------------------------------------------- ##
fence.fig <- ggplot(fence.df) +
  geom_bar(aes(x = year, y = n), stat = "identity") +
  geom_point(aes(x = year, y = n_cumsum), stat = "identity", size = 2) +
  geom_line(aes(x = year, y = n_cumsum, group = 1), size = 0.75, stat = "identity", 
            linetype = "dashed") +
  annotate("text", x = 12, y = 45, label = "Cumulative number of\nborder fences",
           hjust = 0, vjust = 0.5, size = 5) +
  annotate("segment", x = 14.2, xend = 15.7, y = 44, yend = 40) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save
## -------------------------------------------------------------------------- ##
ggsave(fence.fig, filename = "./figures/BorderWalls-Avdan2019.tiff",
       width = 11, height = 8, unit = "in", dpi = 300)

# Cairo pdf
ggsave(fence.fig, filename = "./figures/Figure1_The-increase-in-fortified-borders.pdf",
       width = 11, height = 8, unit = "in", dpi = 300, device = cairo_pdf)
