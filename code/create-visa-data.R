# Visa waiver dataset
# Combine data from 1969/2010/2020

# Data
# year: 2020 scraped in Visa_Scraper.R
# year: 1969 and 2010 retrieved from 
# https://www.fiw.uni-bonn.de/demokratieforschung/personen/laube/visanetworkdata

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "igraph", "readxl")

                      #############################
                      #   Visa Network Data 2020  #
                      #############################

# Load data on visa requirements
# created in Repository "Visa" in file Visa_Scraper.R
### ------------------------------------------------------------------------ ###
visa.df <- import("./data/VWP_07_2020.RDS") %>%
  mutate(requirement = flatten_chr(requirement)) %>%
  ungroup() %>%
  filter(destination_iso3 != "XKX") # Kosovo is already included as "RKS"

# Data cleaning
### ------------------------------------------------------------------------ ###
# (1) Split requirement into information on visa and passport requirements
# (2) Create a binary indicator on visa waivers (1 = visa req. waived; 0 = visa req)
visa.df <- visa.df %>%
  mutate(
    passport_requirement = str_extract(requirement, "(?<=\\n).+"),
    visa_requirement = str_extract(requirement, ".+(?=\\n)"),
    visa_requirement_binary = case_when(
      str_detect(requirement, 
                 "Visa is required|Es ist ein Visum erforderlich|Visa may be obtained on arrival") ~ 0,
      str_detect(requirement, 
                 "Visa is not required") ~ 1,
      TRUE ~ NA_real_),
    across(c("destination_iso3", "nationality_iso3"), ~str_replace(.x, "\\bD\\b", "DEU")))

# Subset
# also exclude Western Sahara (ESH) and Palestine "PSE" due to many missing values
# and unclear international recognition
visa.df <- visa.df %>%
  filter(!destination_iso3 %in% c("ESH", "PSE"), 
         !nationality_iso3 %in% c("ESH", "PSE"))

# Manually add missing information on a few dyads in 2020
# Remaining missing cases: 
# - CAN - SOM
# - CHN - HKG
# - BRA - RKS
# - ZAF - RKS
# - BGD - RKS
# - CHN - MAC
# - CHN - TWN
# - CUB - RKS
# - SRB - RKS

### ------------------------------------------------------------------------###
# CAN -> SOM (visa required)
visa.df[visa.df$destination_iso3 == "CAN" & 
          visa.df$nationality_iso3 == "SOM",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# CHN -> HKG (visa required)
# (Home Return Permit, see: https://en.wikipedia.org/wiki/Visa_requirements_for_Chinese_citizens_of_Hong_Kong)
visa.df[visa.df$destination_iso3 == "CHN" & 
          visa.df$nationality_iso3 == "HKG",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# BRA -> RKS (visa required)
# https://en.wikipedia.org/wiki/Visa_requirements_for_Kosovan_citizens
visa.df[visa.df$destination_iso3 == "BRA" & 
          visa.df$nationality_iso3 == "RKS",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# ZAF -> RKS (visa required)
# https://en.wikipedia.org/wiki/Visa_requirements_for_Kosovan_citizens
visa.df[visa.df$destination_iso3 == "ZAF" & 
          visa.df$nationality_iso3 == "RKS",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# BRA -> CAF (visa required)
visa.df[visa.df$destination_iso3 == "BRA" & 
          visa.df$nationality_iso3 == "CAF",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# ARE -> ISR (visa required / not applicable)
visa.df[visa.df$destination_iso3 == "ARE" & 
          visa.df$nationality_iso3 == "ISR",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# GBR -> NAM (no visa required)
visa.df[visa.df$destination_iso3 == "GBR" & 
          visa.df$nationality_iso3 == "NAM",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# BGD -> RKS (Visa may be obtained on arrival)
# https://en.wikipedia.org/wiki/Visa_requirements_for_Kosovan_citizens
visa.df[visa.df$destination_iso3 == "BGD" & 
          visa.df$nationality_iso3 == "RKS",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa may be obtained on arrival.", 0)

# BLZ -> PAN (visa required)
visa.df[visa.df$destination_iso3 == "BLZ" & 
          visa.df$nationality_iso3 == "PAN",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is not required.", 1)

# CHN -> MAC (visa required)
# (Home Return Permit, see: https://en.wikipedia.org/wiki/Visa_requirements_for_Chinese_citizens_of_Macau)
visa.df[visa.df$destination_iso3 == "CHN" & 
          visa.df$nationality_iso3 == "MAC",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# ARE -> QAT (visa required)
visa.df[visa.df$destination_iso3 == "ARE" & 
          visa.df$nationality_iso3 == "QAT",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# CHN -> TWN (visa required)
# https://en.wikipedia.org/wiki/Visa_policy_of_Taiwan#Chinese_travelers_domiciled_in_Mainland_China
visa.df[visa.df$destination_iso3 == "CHN" & 
          visa.df$nationality_iso3 == "TWN",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# CUB -> RKS (not applicable, admission refused)
# https://en.wikipedia.org/wiki/Visa_requirements_for_Kosovan_citizens
visa.df[visa.df$destination_iso3 == "CUB" & 
          visa.df$nationality_iso3 == "RKS",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# SRB -> RKS (specific regime)
# https://en.wikipedia.org/wiki/Visa_requirements_for_Kosovan_citizens
visa.df[visa.df$destination_iso3 == "SRB" & 
          visa.df$nationality_iso3 == "RKS",
        c("visa_requirement", "visa_requirement_binary")] <- list("Visa is required.", 0)

# Transform into  a network format
### ------------------------------------------------------------------------###
# Create an igraph graph from data frame
visa_2020.graph <- graph_from_data_frame(visa.df[,c("destination_iso3",
                                                    "nationality_iso3",
                                                    "visa_requirement_binary")],
                                         directed = TRUE)

# Transform into  a network format
# Create an igraph graph from data frame
visa_2020.graph <- graph_from_data_frame(visa.df[,c(1,2,6)], directed = TRUE)

# Transform into a matrix
visa_2020.mat <- get.adjacency(visa_2020.graph, sparse = FALSE, 
                               attr = "visa_requirement_binary") 

# Visa Network Data
## -------------------------------------------------------------------------- ##
# retrieved from https://www.fiw.uni-bonn.de/demokratieforschung/personen/laube/visanetworkdata

                       #############################
                       #   Visa Network Data 2010  #
                       #############################

# (1) Load data
visa_2010.df <- read_xls(path = "./data/Visa Network Data_1969_2010.xls",
                         sheet = 2, range = "C5:FN172", 
                         col_types = c("text", rep("numeric", 167)), 
                         na = "/")

# (2) Prepare data
# Delete unnecessary rows and columns
visa_2010.df <- visa_2010.df[-1,]
visa_2010.df <- visa_2010.df[,-2]

# Self-ties are included as "NA", however, they should be coded as "0".
visa_2010.df[is.na(visa_2010.df)] <- 0

# Rename the first column (country IDs) and unambiguous country names
visa_2010.df <- visa_2010.df %>%
  rename(Name = "Home country:", 
         "Central African Republic" = "Central African Rep.", 
         "Comoro Islands" = "Comores Islands",
         "North Korea" = "Korea (Peoples Rep.)",
         "Swaziland" = "Swasiland",
         "Kyrgyzstan" = "Kyrgystan")

# Transform to common ISO3 codes
iso3 <- countrycode(colnames(visa_2010.df)[2:167], "country.name.en", "iso3c")

# As a matrix object
visa_2010.mat <- as.matrix(visa_2010.df[,2:167])

# Use ISO3 codes as row and column names
rownames(visa_2010.mat) <- iso3
colnames(visa_2010.mat) <- iso3

                        #############################
                        #   Visa Network Data 1969  #
                        #############################

# (1) Load data
visa_1969.df <- read_xls(path = "./data/Visa Network Data_1969_2010.xls",
                         sheet = 1, range = "C5:FD161", 
                         col_types = c(rep("text", 3), rep("numeric", 155)), 
                         na = "/")

# (2) Prepare data
# Delete unnecessary rows and columns
visa_1969.df <- visa_1969.df[-1,]
visa_1969.df <- visa_1969.df[,-c(2:3)]

# Self-ties are included as "NA", however, they should be coded as "0".
visa_1969.df[is.na(visa_1969.df)] <- 0

# Rename the first column (country IDs) and unambiguous country names
visa_1969.df <- visa_1969.df %>%
  rename("Name" = "Home country",
         "Republic of the Congo" = "Congo (Brazzaville)",
         "Democratic Republic of the Congo" = "Congo (Kinshasa)",
         "Germany" = "Germany (West)",
         "GDR" = "Germany (East)",
         "Republic of Korea" = "Korea (Rep.)",
         "Democratic People's Republic of Korea" = "Korea (Peoples Rep.)", 
         "Central African Republic" = "Central African Rep.", 
         "Comoro Islands" = "Comores Islands",
         "North Korea" = "Korea (Peoples Rep.)",
         "Swaziland" = "Swasiland",
         "Kyrgyzstan" = "Kyrgystan (U.S.S.R)") %>%
  rename_all(., list(~str_replace(., "\\(.+\\)", "")))

# Transform to common ISO3 codes
iso3 <- countrycode(colnames(visa_1969.df)[2:156], "country.name.en", "iso3c", custom_match = c("GDR" = "DDR"))

# As a matrix object
visa_1969.mat <- as.matrix(visa_1969.df[,2:156])

# Use ISO3 codes as row and column names
rownames(visa_1969.mat) <- iso3
colnames(visa_1969.mat) <- iso3

                       #############################
                       #     Visa Network Data     #
                       #       1969/2010/2020      #
                       #############################

visa.df <- tibble(
  year = c(1969, 2010, 2020),
  data = list(visa_1969.mat,
              visa_2010.mat,
              visa_2020.mat)
  )

# Export
## -------------------------------------------------------------------------- ##
export(visa.df, "./VisaNetworkData.RDS")
