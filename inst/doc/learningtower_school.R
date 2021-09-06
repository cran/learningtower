## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
echo = TRUE,
collapse = TRUE,
comment = "#>",
warning = FALSE,
message = FALSE,
error = FALSE,
outwidth = "100%",
fig.width = 8,
fig.height = 6,
fig.align = "center")

## -----------------------------------------------------------------------------
library(tidyverse)
library(learningtower)

#loading the student subset data 
data(student_subset_2018)

#loading the school data
data(school)

#loading the country data
data(countrycode)

#joining the student, school dataset
school_student_subset_2018 <- left_join(
  student_subset_2018, 
  school, 
  by = c("school_id", "country", "year")) 

#check the count of public and private schools in the a few randomly selected countries
school_student_subset_2018 %>% 
  dplyr::filter(country %in% c("AUS", "QAT", "USA" , "JPN", 
                              "ALB", "PER", "FIN",  "SGP")) %>%
  group_by(country, public_private) %>% 
  tally() %>% 
  dplyr::mutate(percent = n/sum(n)) %>% 
  dplyr::ungroup() %>% 
  left_join(countrycode, by = "country") %>% 
  dplyr::mutate(country_name = fct_relevel(
    country_name, 
    c("Finland", "United States", "Albania", "Peru", "Japan", "Qatar", "Australia"))) %>% 
  ggplot(aes(x = percent,
             y = country_name,
             fill = public_private)) +
  geom_col(position = position_stack()) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#FF7F0EFF", "#1F77B4FF")) +
  labs(title = "Distribution of public and private schools in the year 2018",
       y = "",
       x = "Percentage of schools",
       fill = "")

## ---- echo= FALSE-------------------------------------------------------------
student_data_2018 <- load_student("2018")

data(school)
data(countrycode)

school_student_2018 <- left_join(
  student_data_2018, 
  school, 
  by = c("school_id", "country", "year"))

school_student_2018 %>%
  dplyr::filter(country %in% c("AUS", "QAT", "USA" , "JPN", 
                               "ALB", "PER", "FIN",  "SGP")) %>% 
  group_by(country) %>% 
  summarise(avg_fund_gov = mean(fund_gov, na.rm = TRUE)) %>% 
  arrange(avg_fund_gov) %>% 
  mutate(country = fct_reorder(country, avg_fund_gov)) %>% 
  left_join(countrycode, by = "country") %>% 
  mutate(country_name = fct_reorder(country_name, avg_fund_gov)) %>% 
  ggplot(aes(x=country_name, y=avg_fund_gov)) +
  geom_segment(aes(xend=country_name, yend=0)) +
  geom_point(size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  labs(x = "", 
       y = "Average percentage of government funding", 
       title = "Funding for schools in the year 2018 from government")

