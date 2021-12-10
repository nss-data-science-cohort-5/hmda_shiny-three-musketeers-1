library(tidyverse)
library(sparklyr)
library(httr)
library(jsonlite)
library(glue)

# Create Spark connection.
sc <- spark_connect(master = "local")

# Create queries for FFIEC API pull.
ffiec_queries <- list(states = "WA",
                      years = "2019")

# Set base URL for FFIEC API pull.
ffiec_base_url <- c("https://ffiec.cfpb.gov/v2/data-browser-api/view/csv")

# FFIEC API data, then read as a csv, then copy to Spark.
ffiec_request <- GET(url = ffiec_base_url, 
               query = ffiec_queries)
ffiec_tbl <- read_csv(content(ffiec_request))
ffiec_tbl <- copy_to(sc, ffiec_tbl, overwrite = TRUE)

# Create GLEIF base URL and queries for initial LEI API pull.
gleif_base_url <- c('https://api.gleif.org/api/v1/lei-records')
gleif_base_queries <- list('filter[entity.legalAddress.country]' = 'US',
                           'filter[entity.status]' = 'ACTIVE',
                           'filter[registration.status]' = 'ISSUED',
                           'page[size]' = '200')

gleif_queries <- append(gleif_base_queries, list('page[number]' = '1'))

# API request to get last page, since results are capped at 15 per page.
gleif_request <- GET(url = gleif_base_url,
                     query = gleif_queries,
                     add_headers(Accept = "application/vnd.api+json"))
gleif_tbl <- fromJSON(content(gleif_request, type = "text"))

# Pull the end_point from the last page link of the results and create
# a vector from it.
end_point <- strtoi(str_match(gleif_tbl$links$last, "5D=(\\d+)")[1,2])

page_vector <- c(1:end_point)

# Function for all gleif API pulls.
pull_page_from_gleif_api <- function(page, base_url, base_queries) {
  
  # Same code as before, but with string formatting for each page.
  queries <- append(base_queries, 
                    list('page[number]' = sprintf('%s', 
                                                  page)))
  
  # Pull request from base URL and queries.
  gleif_request <- GET(url = gleif_base_url,
                       query = queries,
                       add_headers(Accept = "application/vnd.api+json"))
  
  # Create initial list from JSON file.
  gleif_function_tbl <- fromJSON(content(gleif_request, type = "text"))
    
  # Here are the categories to pull from the gleif list for this page.
  column_vector <- c("data.id", 
                     "data.attributes.entity.legalName.name")
  
  # Pull those categories into a single column tibble.
  pull_just_one_column <- function(col_name) {
    tbl <- enframe(unlist(gleif_function_tbl)) %>%
      pivot_wider(names_from = name, 
                  values_from = value) %>% 
      select(starts_with(col_name)) %>% 
      pivot_longer(., 
                   everything(),
                   names_to = "number",
                   values_to = col_name) %>% 
      select(-number)
    return(tbl)
    
  }
  
  # Map over all categories then concatenate.
  gleif_function_tbl <- map(column_vector, pull_just_one_column) %>% 
    bind_cols()
  
  return(gleif_function_tbl)
}

# Map over each page and concatenate. Need to fix.
gleif_function_tbl <- map(page_vector,~pull_page_from_gleif_api(.x,
                                                                gleif_base_url,
                                                                gleif_base_queries)) %>% 
  bind_rows()

gleif_function_tbl <- copy_to(sc, gleif_function_tbl, overwrite = TRUE)

# Pull in API key and set census base URL.
api_key <- read_json("data/census.json")
census_base_url <- c("https://api.census.gov/data/2019/acs/acs1/subject")

# Various census variables for collection.
census_stats <- c("NAME",
                  "S0101_C01_001E",
                  "S1810_C01_001E",
                  "S1701_C01_001E",
                  "S2901_C01_007E",
                  "S2901_C01_009E",
                  "S2901_C01_010E",
                  "S2901_C01_011E",
                  "S2901_C01_012E",
                  "S2901_C01_014E",
                  "S2901_C01_015E")

# Put together all variables needed for query.
census_params <- reduce(census_stats, ~paste(.x, .y, sep = ","))

# Query list for census API pull.
census_queries <- list('get' = census_params,
                       'for' = 'county:*',
                       'in'= 'state:53',
                       'key' = api_key)

# Request API data.
census_request <- GET(census_base_url, query = census_queries)

# Pull API data into tibble.
census_tbl <- fromJSON(content(census_request, as = "text"))

# Copy tibble to Spark.
census_tbl <- copy_to(sc, census_tbl, overwrite = TRUE)
