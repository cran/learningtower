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
library(dplyr)
library(ggplot2)
library(learningtower)

#load the subset student data for the year 2018
data(student_subset_2018)
#load the countrycode data
data(countrycode)

glimpse(student_subset_2018)

## -----------------------------------------------------------------------------
student_subset_2018 %>% 
  group_by(country, gender) %>% 
  dplyr::filter(country %in% c("AUS", "QAT", "USA" , "JPN", 
                               "ALB", "PER", "FIN",  "SGP")) %>% 
  dplyr::left_join(countrycode, by = "country") %>% 
  dplyr::mutate(country_name = factor(
    country_name, 
    levels = c("Singapore", "Australia", "Japan", 
                "United States", "Finland", "Albania", "Peru", "Qatar"))) %>% 
  ggplot(aes(x = math,
             y = country_name,
             fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FF7F0EFF", "#1F77B4FF")) +
  theme_classic() +
  labs(x = "Math score", 
       y = "")

## ---- eval=FALSE--------------------------------------------------------------
#  #load the entire student data for the year 2018
#  student_data_2018 <- load_student(2018)
#  
#  #load the entire student data for two/three years (2000, 2012, 2018)
#  student_data_2012_2018 <- load_student(c(2012, 2018))
#  student_data_2000_2012_2018 <- load_student(c(2000, 2012, 2018))
#  
#  #load the entire student
#  student_data_all <- load_student("all")

## ----echo=FALSE---------------------------------------------------------------
student_data_2012_2018 <- load_student(c(2012, 2018))

plot_data <- student_data_2012_2018 %>% 
  group_by(country, year) %>%  
  dplyr::filter(country %in% c("AUS", "QAT", "USA" , "JPN", 
                               "ALB", "PER", "FIN",  "SGP")) %>% 
  dplyr::summarise(avg_math = mean(math, na.rm = TRUE)) %>%  
  left_join(countrycode, by = "country") %>% 
  dplyr::select(country_name, year, avg_math) %>% 
  ungroup() %>% 
  dplyr::mutate(
    year = year %>% as.character %>% as.integer, 
    label_x_pos = ifelse(year == 2012, 2012 - 2, 2018 + 1),
    label = ifelse(
      year == 2012,
      paste0(country_name, ", ", round(avg_math)),
      round(avg_math)))
  
plot_data %>% 
  ggplot(aes(x = year, 
             y = avg_math,
             label = label,
             colour = country_name,
             group = country_name)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept=2012,
             linetype="dashed",
             size=0.1) +
  geom_vline(xintercept=2018,
             linetype="dashed",
             size=0.1) + 
  geom_text(aes(x = label_x_pos),
            position = position_nudge(y = 0)) +
  scale_x_continuous(breaks = c(2012, 2018),
                     limits = c(2008, 2020)) +
  scale_colour_manual(values = c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", 
                                 "#9467BDFF", "#8C564BFF", "#E377C2FF", "#7F7F7FFF")) +
  labs(x = "",
       y = "Average maths score") +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

