library(shiny)
library(tidyverse)
library(DT)

library(scales)


hmda_lei_census <- read_csv("data/hmda_lei_census.csv")

age_levels <- c("<25", 
                "25-34", 
                "35-44", 
                "45-54", 
                "55-64", 
                "65-74", 
                ">74")


filter_age <- function(reactive) {
  co_applicant <- reactive %>% 
    rename("co_applicant_age" = "co-applicant_age") %>% 
    count(co_applicant_age = factor(co_applicant_age,levels = age_levels)) %>% 
    drop_na()
  
  applicant_vars <- reactive %>% 
    count(applicant_age = factor(applicant_age, levels = age_levels)) %>% 
    drop_na() %>% 
    bind_cols(co_applicant) %>% 
    select(-co_applicant_age) %>% 
    rename("n1" = "n...2", "n2" = "n...4") %>% 
    mutate("Total" = rowSums(.[2:3])) %>% 
    select(-n1, -n2) %>% 
    rename("Age_Group" = "applicant_age")
  
  area_vars <- reactive %>%
    select(starts_with("Area")) %>%
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
    mutate(Category = recode(Category, "Area_Total" = "Area Total"))
  
  return(age_vars)
}




hmda_lei_census <- read_csv("../data/hmda_lei_census.csv")



# hmda_lei_census %>% 
#   select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>% 
#   group_by(Name) %>%
#   summarise(mean(White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino))
# 
# hmda_lei_census %>% 
#   select(Name, White, `African-American`, `Native American`, Asian, `Multi-Racial`, Latino) %>% 
#   group_by(Name) %>%
#   summarise(
#     White = mean(White),
#     `African-American` = mean(`African-American`), 
#     `Native American`= mean(`Native American`), 
#     Asian = mean(Asian), 
#     `Multi-Racial`= mean(`Multi-Racial`), 
#     Latino = mean(Latino)) %>% 
#   t()






  