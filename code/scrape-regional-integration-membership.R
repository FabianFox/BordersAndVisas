# Regional integration

# Data
# data from: https://riks.cris.unu.edu/

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "rvest", "janitor")

# Regional integration entities
### ------------------------------------------------------------------------ ###
integration.df <- tibble(
  project_name = c("ASEAN", "EAC", "ECOWAS", "EU", "MERCOSUR", "NAFTA", "SADC",
                   "CAIS"),
  link = str_c(
    "https://riks.cris.unu.edu/country-organisation?filter=organisation&organisations=", 
    project_name)
)

# Get information on members
# Function that scrapes the information
scrape_fun <- function(x){
  read_html(x) %>%
    html_element(".result__table") %>%
    html_table() %>%
    clean_names() %>%
    mutate(range = str_squish(range))
}

# Safe scraping
scrape_fun <- possibly(scrape_fun, otherwise = NA)

# Get information
integration.df <- integration.df %>%
  mutate(membership = map(link, ~scrape_fun(.x)))

# Unnest information
integration_long.df <- integration.df %>%
  unnest(membership) %>%
  select(-link) %>%
  mutate(project_name = str_replace_all(project_name, "CAIS", "SICA"),
         country = countrycode(country, "country.name.en", "iso3c"),
         joined = strtoi(str_extract(range, "[:digit:]{4}(?=\\s-)")),
         left = str_extract(range, "(?<=-\\s).+"))

# Export
### ------------------------------------------------------------------------ ###
export(integration_long.df, "./data/regional_integration.RDS")
