library(tidyverse)
library(httr)
library(jsonlite)

# Create queries for FFIEC API pull.
hmda_queries <- list(states = "WA",
                      years = "2019")

# Set base URL for FFIEC API pull.
hmda_base_url <- c("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv")

# FFIEC API data, then read as a csv.
hmda_request <- GET(url = hmda_base_url, query = hmda_queries)

hmda <- read_csv(content(hmda_request))

# Method from .csv file, and then filter by US and only keep LEI and name.
lei <- read_csv("data/gleif.csv",
                               col_select = c("LEI",
                                              "Entity.LegalName",
                                              "Entity.LegalAddress.Country",
                                              "Entity.HeadquartersAddress.Region")) %>%
filter(Entity.LegalAddress.Country == "US") %>%
  select(LEI, Entity.LegalName)

# Adjust LEI tibble column names.
colnames(lei) <- c("LEI", "Entity Name")

# Pull in API key and set census base URL.
api_key <- read_json("data/census.json")
census_base_url <- c("https://api.census.gov/data/2019/acs/acs1/subject")

# Various census variables for collection.
census_stats <- c("NAME",
                  # Total Population
                  "S0101_C01_001E",
                  # Disability
                  "S1810_C01_001E",
                  # Poverty Status
                  "S1701_C01_001E",
                  # Males 18+
                  "S2901_C02_006E",
                  # Females 18+
                  "S2901_C01_007E",
                  # African American 18+
                  "S2901_C01_009E",
                  # Asian 18+
                  "S2901_C01_010E",
                  # Native American
                  "S2901_C01_011E",
                  # Multi-racial
                  "S2901_C01_014E",
                  # Latino
                  "S2901_C01_015E")

# Put together all variables needed for query.
census_params <- reduce(census_stats, ~paste(.x, .y, sep = ","))

# Query list for census API pull.
census_queries <- list('get' = census_params,
                       'for'= 'county:*',
                       'in' = 'state:53',
                       'key' = api_key)

# Request API data.
census_request <- GET(census_base_url, query = census_queries)

# Pull API data into tibble.
census <- fromJSON(content(census_request, as = "text")) %>% 
  as_tibble()

# Create census column names and rename census tibble accordingly.
census_names <- c("Name",
                 "Total Population",
                 "Disability",
                 "Poverty Status",
                 "Males 18+",
                 "Females 18+",
                 "African American 18+",
                 "Asian 18+",
                 "Native American",
                 "Multi-racial",
                 "Latino",
                 "State",
                 "County")
colnames(census) <- census_names

# Remove first row.
census <- census %>% 
  slice(-1)

# Change all numeric values to numeric type.
census[,2:11] <- lapply(2:11, function(x) as.numeric(census[[x]]))

# Replace bad values with NAs.
census <- census %>% 
  mutate_all(function(x) replace(x, which(x < 0), NA))
