library(tidyverse)
library(httr)
library(jsonlite)
library(splitstackshape)

# Create queries for FFIEC API pull.
hmda_queries <- list(states = "WA",
                      years = "2019")

# Set base URL for FFIEC API pull.
hmda_base_url <- c("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv")

# FFIEC API data, then read as a csv.
hmda_request <- GET(url = hmda_base_url, query = hmda_queries)

hmda <- read_csv(content(hmda_request))

# Obtain LAR key for data codes. Downloaded from CFPB GitHub repol
lar <- read_csv("data/lar.csv",  
                col_select = c("Data Field Name", 
                               "Descriptions and Examples",
                               "Valid Values"))
# Rename columns.
names(lar) <- c("Name", "Descriptions", "VVs")

# Just need to reduce the tibble down to variables for which categorical
# numbers are used as "keys".
lar <- lar %>% 
  filter(grepl("^Description", Descriptions))

# Split the valid values by spaces long-ways.
lar <- cSplit(indt = lar, 
              splitCols = "VVs", 
              direction = "long", 
              sep = " ")

# Change valid values to character column.
lar <- lar %>% 
  mutate(VVs = as.character(VVs)) %>% 
  filter(VVs != "1111")

# Split "1.C" or "1.H" instances into "1. C" and "1. H".
lar$Descriptions <- gsub("(\\d.)([A-Z])", "\\1 \\2", lar$Descriptions)

# Compares pairs of values in two columns and extracts appropriate value
# For each "key".
for(i in (1:length(lar$Descriptions))) {
  if(str_detect(lar$Descriptions[i], lar$VVs[i])) {
    # Find a match? Pull out everything up to that point.
    lar$Descriptions[i] <- str_extract(lar$Descriptions[i],
                                        sprintf("(?<=%s.\\s).*",lar$VVs[i]))
    # Then remove additional options afterwards.
    lar$Descriptions[i] <- ifelse(str_detect(lar$Descriptions[i],
                                              "[0-9]"),
                                   str_extract(lar$Descriptions[i],
                                               "^.+?(?=\\s\\d{1,2}\\.\\s)"),
                                   lar$Descriptions[i])
    # Then remove a couple extra sentences following certain options.
    lar$Descriptions[i] <- ifelse(str_detect(lar$Descriptions[i],
                                              "\\.\\s"),
                                   str_extract(lar$Descriptions[i],
                                               "^[A-Za-z]+?(?=\\.\\s)"),
                                   lar$Descriptions[i])
    
  }
}

# Need to fix.
find_a_match_in_lar <- function(cell, lar, guidepost) {
  for(i in (1:length(lar$VVs))) {
    if((lar$Name[i] == guidepost) && (cell == lar$VVs[i])) {
      cell <- lar$Descriptions[i]
    }
  }
  return(cell)
}

for(i in (1:length(lar$Name))){
  hmda %>% 
    select(contains(lar$Name[i])) %>% 
    apply(., 1, function(x) find_a_match_in_lar(x, lar, lar$Name[i]))
}

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
# Add tracts to census.
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
