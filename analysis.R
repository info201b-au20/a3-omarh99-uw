library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(lintr)

# Load data set
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

### Step 2: Filter the incarceration of black population ages 15-64
inc_data <- data %>%
  select(total_pop, black_pop_15to64, total_pop_15to64, year, fips, state,
         county_name) %>%
  mutate(black_perc_15to64 = black_pop_15to64 / total_pop_15to64) %>%
  filter(year == 2018)

## Summary Information Questions
summary_info <- list()

summary_info$highest_fip <- inc_data %>%
  filter(year == 2018) %>%
  filter(fips == max(fips)) %>%
  pull(county_name)

summary_info$state_max_fip <- inc_data %>%
  filter(year == 2018) %>%
  filter(fips == max(fips)) %>%
  pull(state)

summary_info$max_black_pop <- inc_data %>%
  filter(year == 2018) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(black_pop_15to64)

summary_info$max_black_state <- inc_data %>%
  filter(year == 2018) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(state)

summary_info$max_black_county <- inc_data %>%
  filter(year == 2018) %>%
  filter(total_pop == max(total_pop)) %>%
  pull(county_name)
 
 # Trend over time chart comparing two states
state_inmate_prop <- data %>%
  na.omit() %>%
  select(fips, state, county_name, black_jail_pop_rate) %>%
  rename(states = state) %>%
  group_by(states) %>%
  filter(states == "KY" | states == "TX")

data_plot <- ggplot(data = state_inmate_prop, aes(x = states,
                                                  y = black_jail_pop_rate)) +
 geom_boxplot(outlier.shape = NA) +
 ylim(NA, 10000) +
labs(title = "Different States Black Jail Populations", x = "States",
     y = "Rate per 100,000")

### Step 3: Create data table of map data w/ fips code
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

### Step 4: Merge the map data with black pop 15-64 incarcerated data
map_data <- county_shapes %>%
  left_join(inc_data, by = "fips") %>%
  filter(state == "TX")

### Step 5: Create a blank canvas for map
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

### Step 6: Create the map
 ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_perc_15to64),
    color = "white", size = 0.2
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_perc_15to64)),
                        na.value = "white") +
  blank_theme +
  ggtitle("Populations per county in TX")
