# Analyze: Border Infrastructure Data

# Load/install pkgs
## -------------------------------------------------------------------------- ##
if(!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "lemon", "janitor", "countrycode")

# Load: Border Infrastructure Data
## -------------------------------------------------------------------------- ##
# Indicator
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
                       sheet = 1, na = "NA") %>%
  as_tibble() %>%
  select(1:3, 16) %>%
  filter(!is.na(typology),
         !(state1 == "ARE" & state2 == "QAT"),        # no shared border (since 1974) 
         !(state1 == "QAT" & state2 == "ARE")) %>%    # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE)

# Join indicator data
# duplicates because some countries share multiple borders, i.e. RUS/CHN
border.df <- indicator.df %>%
  mutate(
    continent1 = countrycode(state1, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    continent2 = countrycode(state2, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    region1 = countrycode(state1, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")),
    region2 = countrycode(state2, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")))

# Dirty solution to create a directed dyadic typology
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- border.df %>%
  select("state2", "state1", "typology") %>%
  rename(
    state1 = state2,
    state2 = state1,
    state2_typology = typology
  )

# (2) Join to original dataset
border.df <- border.df %>%
  left_join(swap.df) %>%
  rename(state1_typology = typology)

# Define factor variables and style scheme
## -------------------------------------------------------------------------- ##
# (1) Indicator factor levels
fac_ind_en <- function(x) {
  factor(x, levels = c("frontier border", "landmark border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("'No man's land'","Landmark", "Checkpoint", 
                    "Barrier", "Fortified"))
}

# (2) Theme for the plots
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(colour = "black", size = 12)
  )

# Exploratory analysis of indicator
### ------------------------------------------------------------------------ ###
# Distribution of indicator (in %)
ind_perc.df <- border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100,
         continent1 = "World")

# Round global distribution
border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100, continent1 = "World") %>%
  mutate(rounded_perc = round(perc, digit = 1)) %>%
  arrange(fac_ind_en(state1_typology))

# Only the global distribution
ind_perc.fig <- ind_perc.df %>%
  ggplot(mapping = aes(x = state1_typology, y = perc)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Distribution of indicator across continents + global
ind_perc_region.fig <- border.df %>%
  mutate(continent1 = if_else(state1 == "PNG", "Asia", continent1)) %>%
  group_by(continent1, state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100) %>% 
  select(-group_n) %>%
  bind_rows(ind_perc.df) %>%
  ggplot(mapping = aes(x = fac_ind_en(state1_typology), y = perc)) +
  geom_bar(stat = "identity") +
  # 'hacky' solution to create a custom legend
  geom_point(aes(shape = fac_ind_en(state1_typology)), alpha = 0) + 
  guides(shape = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  scale_x_discrete(name = "", labels = c("N","L", "C", "B", "F")) +
  scale_shape_manual(name = "Legend", values = c("N","L", "C", "B", "F")) +
  # 
  geom_text(stat = "identity", aes(label = paste0("N = ", count)), vjust = -0.3,
            size = 4) +
  facet_rep_wrap(~factor(continent1, 
                         levels = c("Africa", "Americas", "Asia", "Europe", "World"),
                         labels = c("Africa", "North & South America", "Asia (incl. Oceania)", 
                                    "Europe", "Global distribution")),
                 repeat.tick.labels = "bottom") +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        strip.text = element_text(size = 14),
        panel.spacing.y = unit(5, "lines"),
        legend.position = c(.85, 0.25),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .1)))

# Round global distribution (regions)
global_dist.df <- border.df %>%
  mutate(continent1 = if_else(state1 == "PNG", "Asia", continent1)) %>%
  group_by(continent1, state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100) %>%
  mutate(rounded_perc = round(perc, digit = 1)) %>%
  select(continent1, state1_typology, rounded_perc) %>%
  pivot_wider(names_from = continent1, values_from = rounded_perc, values_fill = 0) %>%
  arrange(fac_ind_en(state1_typology))

# Total of barriers and fortified borders
# most prevalent in Asia
global_dist.df %>%
  filter(state1_typology %in% c("barrier border", "fortified border")) %>%
  bind_rows(summarise(., 
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total")))

# Export
## -------------------------------------------------------------------------- ##
ggsave(
  plot = ind_perc_region.fig, "./figures/Fig1 - Typology By Region.tiff", 
  width = 10, height = 8, unit = "in",
  dpi = 300
)
