library(shiny)
library(tidyverse)
library(DT)



hmda_lei_census <- read_csv("data/hmda_lei_census.csv")

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





  