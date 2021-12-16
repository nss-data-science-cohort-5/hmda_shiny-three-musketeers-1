# Libraries and read in data to go into global.R.
library(tidyverse)

hmda_lei_census <- read_csv("data/hmda_lei_census.csv")

# Levels to be inputted into global.R.
age_levels <- c("<25", 
                "25-34", 
                "35-44", 
                "45-54", 
                "55-64", 
                "65-74", 
                ">74")

# Barplot of percentages of applicant ages to be inputted into Server.R.
hmda_lei_census %>%
  count(applicant_age = factor(applicant_age, levels = age_levels)) %>%
  mutate(pct = prop.table(n)) %>%
  drop_na() %>% 
  ggplot(aes(x = applicant_age, 
             y = pct, 
             fill = applicant_age, 
             label = scales::percent(pct))) +
  geom_col() +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Applicant Age Group",
       y = "Percentage of Applicants",
       title = "Applicant Age Group Percentage") +
  theme(legend.position = "None")

# Barplot of counts of applicant ages to be inputted into Server.R.
hmda_lei_census %>%
  select(applicant_age) %>% 
  mutate(applicant_age = factor(applicant_age, levels = age_levels)) %>% 
  drop_na() %>% 
  ggplot(aes(x = applicant_age, 
             fill = applicant_age)) +
  geom_bar() + 
  labs(x = "Applicant Age Group",
       y = "Number of Applicants",
       title = "Applicant Age Group Counts") +
  theme(legend.position = "None")

# Table of percentages and counts
# of applicant ages to be inputted into Server.R.
# Use for both output 1 and output 2.
hmda_lei_census %>%
  count(applicant_age = factor(applicant_age, levels = age_levels)) %>%
  drop_na() %>% 
  mutate(Percent = round(n/sum(n)*100,2)) %>%
  rename("Applicant Age Group" = applicant_age,
         "Number of Applicants" = n)
