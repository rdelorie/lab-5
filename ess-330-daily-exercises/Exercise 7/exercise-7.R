# Rachel Delorie, 22-Feb-2025, Exercise 7 for ESS 330
library(tidyverse)
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# question 1

# 6 states with most covid cases
topStates = covid_data %>% 
  slice_max(date) %>% 
  group_by(state) %>% 
  summarize(tot = sum(cases)) %>% 
  slice_max(tot, n =6) %>% 
  pull(state)

# states in our raw data -> top 6 -> we want state name and date, then categorized
d = covid_data %>% 
  filter(state %in% topStates) %>% 
  group_by(state, date) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup()
# plot info from above
ggplot(d, aes(x = date, y = cases, col = state)) +
  geom_line() +
  facet_wrap(~state) +
  theme_classic()

# question 2

#find max date, then group that date together. summarize adds all cases together
daily_total <- covid_data %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases))

# plot info from above
ggplot(daily_total, aes(x = date, y = cases, col(date))) + 
  geom_line() +
  theme_classic()