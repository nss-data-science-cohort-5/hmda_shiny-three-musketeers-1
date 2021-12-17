# Libraries and read in data to go into global.R.
library(tidyverse)
library(scales)

hmda_lei_census <- read_csv("data/hmda_lei_census.csv")

# Levels to be inputted into global.R.
age_levels <- c("<25", 
                "25-34", 
                "35-44", 
                "45-54", 
                "55-64", 
                "65-74", 
                ">74")

# Find totals for applicants and co-applicants, then combine the two.
co_applicant <- hmda_lei_census %>% 
  rename("co_applicant_age" = "co-applicant_age") %>% 
  count(co_applicant_age = factor(co_applicant_age,levels = age_levels)) %>% 
  drop_na()

applicant_vars <- hmda_lei_census %>% 
  count(applicant_age = factor(applicant_age, levels = age_levels)) %>% 
  drop_na() %>% 
  bind_cols(co_applicant) %>% 
  select(-co_applicant_age) %>% 
  rename("n1" = "n...2", "n2" = "n...4") %>% 
  mutate("Total" = rowSums(.[2:3])) %>% 
  select(-n1, -n2) %>% 
  rename("Age_Group" = "applicant_age")

# Find totals for areas select (implicitly because of the reactive filter).
area_vars <- hmda_lei_census %>%
  select(starts_with("Area")) %>%
  distinct() %>% 
  drop_na() %>% 
  mutate_all(sum) %>% 
  .[1,] %>% 
  pivot_longer(data = .,
               cols = everything(),
               names_to = "Area Age Group",
               values_to = "Area_Total")

# Combine both sets of total age data for each group and calculate percentages.
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

# Barplot of percentages of applicant ages to be inputted into Server.R.
age_vars %>% 
  filter(grepl("Percentage", Category)) %>% 
  ggplot(aes(x = Group, 
             y = Value, 
             fill = Category, 
             label = percent(Value))) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(position = position_dodge(width = 1),
            vjust = -0.5,
            size = 4) +
  scale_y_continuous(labels = percent,
                     limits = c(0,0.35)) +
  labs(x = "Applicant Age Group",
       y = "Percentage of Applicants",
       title = "Applicant Age Group Percentage")

# Barplot of counts of applicant ages to be inputted into Server.R.
age_vars %>% 
  filter(grepl("Total", Category)) %>% 
  ggplot(aes(x = Group, 
             y = Value, 
             fill = Category)) +
  geom_col(position = position_dodge(width = 1)) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0,3000000,by=250000),
                     limits = c(0,2500000)) +
  labs(x = "Applicant Age Group",
       y = "Number of Applicants",
       title = "Applicant Age Group Totals")

# Table of percentages and counts
# of applicant ages to be inputted into Server.R.
# Use for both output 1 and output 2.
age_vars %>%
  pivot_wider(names_from = "Category", values_from = "Value") %>% 
  mutate("Applicant Percentage" = sapply(.[["Applicant Percentage"]],
                                         label_percent()),
         "Area Percentage" = sapply(.[["Area Percentage"]],
                                    label_percent())) %>%
  rename("Applicant Age Group" = "Group", "Applicant Total" = "Total") %>% 
  mutate("Area Total" = prettyNum(.[["Area Total"]], big.mark = ","),
         "Applicant Total" = prettyNum(.[["Applicant Total"]], big.mark = ",")) %>% 
  relocate("Applicant Total", .before = "Applicant Percentage")
