# Load the `dplyr` package
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(plotly)
library(tidyr)
library(lintr)

# Load your data, making sure to not interpret strings as factors
amazon_data <- read.csv("amazon.csv", stringsAsFactors = FALSE)
ff_df <- data.frame(amazon_data, stringsAsFactors = FALSE)
ff_df$date <- as.Date(ff_df$date)

# - What are the column names?
colnames(ff_df)

# - How many rows is the data frame?
nrow(ff_df)

# - How many columns are in the data frame?
ncol(ff_df)

# Use the `summary` function to get some summary information
summary(ff_df)

# How many events occurred?
total_events <- ff_df %>%
  nrow()

# How many total fires were there?
total_fires <- ff_df %>%
  summarise(sum(number)) %>%
  floor()

# Which state was most impacted by fires?

state_impacted <- ff_df %>%
  group_by(state) %>%
  summarise(impacted = sum(number)) %>%
  filter(max(impacted) == impacted) %>%
  pull(state)

# Which year was most impacted by fires?

year_impacted <- ff_df %>%
  group_by(year) %>%
  summarise(impacted = sum(number)) %>%
  filter(max(impacted) == impacted) %>%
  pull(year)

# Which date had the most impact?
worst_date <- ff_df %>%
  filter(number == max(number)) %>%
  pull(date)

# How many fires on that day?
worst_day <- ff_df %>%
  group_by(state) %>%
  filter(date == worst_date) %>%
  pull(state)

# PLOT

df <- ff_df %>%
  select(year, number) %>%
  group_by(year) %>%
  mutate(number = sum(number)) %>%
  gather(key = "variable", value = "value", -year)
head(df, 1)

ggplot(df, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#E7B800")) +
  labs(title = "Forest Fires in Brazil from 1998 to 2017") +
  ylab("number of forest fires") +
  theme_minimal()
