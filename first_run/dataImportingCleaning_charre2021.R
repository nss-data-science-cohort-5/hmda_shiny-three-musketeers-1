library(tidyverse)
library(httr)
library(jsonlite)
library(splitstackshape)
library(sf)

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

# Conduct a string replace to better match lar values to hmda columns.
patterns <- c("applicant or borrower - name and version 
              of credit scoring model",
              "co-applicant or co-borrower - name and version 
              of credit scoring model",
              "automated underwriting system",
              "reason for denial",
              "initially payable to your institution",
              "other non-amortizing features",
              "type of purchaser",
              "sex of co-applicant or co-borrower collected 
              on the basis of visual observation or surname",
              "sex of applicant or borrower collected 
              on the basis of visual observation or surname",
              "sex of co-applicant or co-borrower",
              "sex of applicant or borrower",
              "race of co-applicant or co-borrower collected on the basis 
              of visual observation or surname",
              "race of applicant or borrower collected on the basis 
              of visual observation or surname",
              "race of co-applicant or co-borrower",
              "race of applicant or borrower",
              "ethnicity of co-applicant or co-borrower 
              collected on the basis of visual observation or surname",
              "ethnicity of applicant or borrower
              collected on the basis of visual observation or surname",
              "ethnicity of co-applicant or co-borrower",
              "ethnicity of applicant or borrower",
              ": ",
              " ")

replacements <- c("applicant_credit_score_type",
                  "co_applicant_credit_score_type",
                  "aus",
                  "denial reason",
                  "initially_payable_to_institution",
                  "other nonamortizing features",
                  "purchaser type",
                  "co-applicant sex observed",
                  "applicant sex observed",
                  "co-applicant sex",
                  "applicant sex",
                  "co-applicant race observed",
                  "applicant race observed",
                  "co-applicant race",
                  "applicant race",
                  "co-applicant ethnicity observed",
                  "applicant ethnicity observed",
                  "co-applicant ethnicity",
                  "applicant ethnicity",
                  "-",
                  "_")

# Replacement function.
lc_replace_and_underscore <- function(given_string) {
  given_string <- tolower(given_string)
  for(i in (1:length(patterns))) {
    if(str_detect(given_string, substring(patterns[i], 1, 6))) {
      given_string <- str_replace_all(given_string, 
                                      patterns[i], 
                                      replacements[i])
    }
  }
  return(given_string)
}

# Cycle through lar$Name and string replace.
lar$Name <- sapply(lar$Name, lc_replace_and_underscore)
lar$VVs <- sapply(lar$VVs, as.numeric)

# Pull out matching HMDA column indices from H
vec_of_hmda_columns <- c()
vec_of_hmda_column_indices <- c()

# Pull out column names where there are matches in lar.
for(i in (1:length(lar$Name))){
  for(j in (1:length(hmda))){
    if(names(hmda)[j] == lar$Name[i]) {
      vec_of_hmda_columns <- c(vec_of_hmda_columns, names(hmda)[j])
    }}}

# Pull out unique values of name vector.
unique_vec_of_hmda_columns <- unique(vec_of_hmda_columns)

# Break hmda into two tibbles based on name vector.
# Refactor to try to remove these extra copies if time.
hmda2 <- hmda %>% 
  select(all_of(unique_vec_of_hmda_columns)) %>% 
  mutate_all(as.numeric)

hmda3 <- hmda %>% 
  select(-all_of(unique_vec_of_hmda_columns))

# Replace categorical number values in hmda2 with descriptions in lar
# via subsetting both datasets and a left join.
hmda <- map(names(hmda2), function(hmda2_name) {
  
  addl_lar <- lar %>% 
    filter(lar$Name == hmda2_name) %>% 
    rename_with(function (x) {
      x <- hmda2_name
    },
    .cols = VVs)

  new_hmda2_col <- hmda2 %>%
    select(hmda2_name) %>% 
    left_join(addl_lar) %>%
    select(Descriptions) %>%
    rename_with(function (x) {
      x <- hmda2_name
    },
    .cols = Descriptions)
  
}) %>% 
  bind_cols() %>% 
  bind_cols(hmda3)

# Prepare hmda for merge.
hmda <- hmda %>% 
  mutate(census_tract = as.character(census_tract - (county_code * 1000000))) %>% 
  mutate(census_tract = map_chr(census_tract, function(x) {
    if_else(nchar(x) < 6, sprintf("0%s", x), x)}))

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
census_base_url <- c("https://api.census.gov/data/2019/acs/acs5/subject")

# Various census variables for collection.
census_stats <- c("NAME",
                  # Total Population
                  "S0101_C01_001E",
                  # Age Under 5
                  "S0101_C01_002E",
                  # Age 5 - 9
                  "S0101_C01_003E",
                  # Age 10 - 14
                  "S0101_C01_004E",
                  # Age 15 - 19
                  "S0101_C01_005E",
                  # Age 20 - 24
                  "S0101_C01_006E",
                  # Age 25 - 29
                  "S0101_C01_007E",
                  # Age 30 - 34
                  "S0101_C01_008E",
                  # Age 35 - 39
                  "S0101_C01_009E",
                  # Age 40 - 44
                  "S0101_C01_010E",
                  # Age 45 - 49
                  "S0101_C01_011E",
                  # Age 50 - 54
                  "S0101_C01_012E",
                  # Age 55 - 59
                  "S0101_C01_013E",
                  # Age 60 - 64
                  "S0101_C01_014E",
                  # Age 65 - 69
                  "S0101_C01_015E",
                  # Age 70 - 74
                  "S0101_C01_016E",
                  # Age 75 - 79
                  "S0101_C01_017E",
                  # Age 80 - 84
                  "S0101_C01_018E",
                  # Age 85+
                  "S0101_C01_019E",
                  # Disability
                  "S1810_C01_001E",
                  # Poverty Status
                  "S1701_C01_001E",
                  # Males
                  "S0101_C03_001E",
                  # Females
                  "S0101_C05_001E")

# Put together all variables needed for query.
census_params <- reduce(census_stats, ~paste(.x, .y, sep = ","))

# Query list for census API pull.
census_queries <- list('get' = census_params,
                       'for'= 'tract:*',
                       'in' = 'state:53',
                       'in' = 'county:*',
                       'key' = api_key)

# Request API data.
census_request <- GET(census_base_url, query = census_queries)

# Pull API data into tibble.
census <- fromJSON(content(census_request, as = "text")) %>% 
  as_tibble()

census

# Create census column names and rename census tibble accordingly.
census_names <- c("Name",
                  "Total Population",
                  "Age Under 5",
                  "Age 5 - 9",
                  "Age 10 - 14",
                  "Age 15 - 19",
                  "Age 20 - 24",
                  "Age 25 - 29",
                  "Age 30 - 34",
                  "Age 35 - 39",
                  "Age 40 - 44",
                  "Age 45 - 49",
                  "Age 50 - 54",
                  "Age 55 - 59",
                  "Age 60 - 64",
                  "Age 65 - 69",
                  "Age 70 - 74",
                  "Age 75 - 79",
                  "Age 80 - 84",
                  "Age 85+",
                  "Disability",
                  "Poverty Status",
                  "Males",
                  "Females",
                  "State",
                  "County",
                  "Tract")

colnames(census) <- census_names

# Remove first row.
census <- census %>% 
  slice(-1)

# Change all numeric values to numeric type.
census[,2:24] <- lapply(2:24, function(x) as.numeric(census[[x]]))

# Replace bad values with NAs.
census <- census %>% 
  mutate_all(function(x) replace(x, which(x < 0), NA))

# Read in shapefile of census tracts in WA.
wash <- read_sf("data/tl_2021_53_tract.shp")

# Join all dataframes.
# Consider dropping duplicative county/state columns and designating columns as
# belonging to particular dataframes.
# Inspect joins on census tracts to make sure appropriate data is captured.
hmda_lei_census <- hmda %>% 
  left_join(lei, by = c("lei" = "LEI")) %>% 
  left_join(census, by = c("census_tract" = "Tract")) %>% 
  left_join(wash, by = c("census_tract" = "TRACTCE"))

# Write to .csv file.
write_csv(hmda_lei_census, file = "data/hmda_lei_census.csv")
