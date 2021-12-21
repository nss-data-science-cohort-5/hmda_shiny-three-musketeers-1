library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(scales)
library(sf)
library(leaflet)
library(htmltools)

hmda_lei_census <- read_csv("data/hmda_lei_census.csv")
first_map_data <- read_sf("data/wash.shp")
ll_map_data <- read_sf("data/ll_map_data.shp")

age_levels <- c("25-34", 
                "35-44", 
                "45-54", 
                "55-64", 
                "65-74", 
                ">74")

filter_age <- function(reactive) {
  co_applicant <- reactive %>% 
    rename("co_applicant_age" = "co-applicant_age") %>% 
    filter(co_applicant_age != "<25") %>% 
    count(co_applicant_age = factor(co_applicant_age,levels = age_levels)) %>% 
    drop_na()
  
  applicant_vars <- reactive %>% 
    filter(applicant_age != "<25") %>% 
    count(applicant_age = factor(applicant_age, levels = age_levels)) %>% 
    drop_na() %>% 
    bind_cols(co_applicant) %>% 
    select(-co_applicant_age) %>% 
    rename("n1" = "n...2", "n2" = "n...4") %>% 
    mutate("Total" = rowSums(.[2:3])) %>% 
    select(-n1, -n2) %>% 
    rename("Age_Group" = "applicant_age")
  
  area_vars <- reactive %>%
    select(starts_with("Area"), -("Area <25")) %>%
    distinct() %>% 
    drop_na() %>% 
    mutate_all(sum) %>% 
    .[1,] %>% 
    pivot_longer(data = .,
                 cols = everything(),
                 names_to = "Area Age Group",
                 values_to = "Area_Total")
  
  age_vars <- bind_cols(area_vars, applicant_vars) %>% 
    mutate("Applicant Percentage" = prop.table(Total),
           "Area Percentage" = prop.table(Area_Total)) %>% 
    pivot_longer(data = ., 
                 cols = c(ends_with("Percentage"), ends_with("Total")), 
                 names_to = "Category",
                 values_to = "Value") %>% 
    select(-("Area Age Group")) %>% 
    rename("Group" = "Age_Group") %>% 
    mutate(Category = recode(Category, 
                             "Area_Total" = "Area Total",
                             "Total" = "Applicant Total"))
  
  return(age_vars)
}

filter_map <- function(reactive) {
  
  population <- reactive %>% 
    select("Name", "Total Population") %>% 
    mutate(Name = str_remove(Name, " County, Washington")) %>% 
    rename("Total_Population" = "Total Population") %>% 
    distinct(Name, Total_Population)
  
  map_data <- reactive %>% 
    count(Name) %>% 
    rename(Aggregate_Number = n) %>% 
    mutate(Name = str_remove(Name, " County, Washington")) %>% 
    full_join(population) %>% 
    rowwise() %>% 
    mutate(Aggregate_Number = round(Aggregate_Number/Total_Population * 1000)) %>% 
    ungroup() %>%
    drop_na() %>% 
    mutate(Pct_Aggregate_Number = Aggregate_Number/sum(Aggregate_Number)) %>% 
    full_join(first_map_data, by = c("Name" = "NAME")) %>% 
    fill(0) %>% 
    st_as_sf()
  
  # Pull out centroids as shapes in separate column using sf.
  map_data$centroids <- map_data %>% 
    st_centroid() %>% 
    st_geometry()
  
  # Pull out separate coordinates as columns from dataset.
  map_data <- map_data %>% 
    mutate(lat = unlist(map(map_data$centroids,1)),
           long = unlist(map(map_data$centroids,2)))
  
  return(map_data)
}


# Labels for leaflet plot.
labels <- paste(
  "<strong>",ll_map_data$Name,"</strong><br/>",
  "Failed Applications Per 1,000 People: ",ll_map_data$COAT,"<br/>",
  "Percentage POC: ",ll_map_data$POC,"%<br/>",
  "Percentage Women: ",ll_map_data$Females,"%<br/>"
) %>% lapply(htmltools::HTML)

pal <- colorBin(
  palette = "YlOrRd",
  domain = log2(ll_map_data$COAT),
  n = 10
)
