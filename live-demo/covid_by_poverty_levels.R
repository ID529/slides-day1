# visualization of covid-19 county mortality rates by poverty level
#
# this script will follow the following steps:
#
# - declare dependencies
# - load the covid-19 data
# - clean it
# - retrieve poverty data from the Census
# - clean it
# - merge the datasets
# - visualize county rates over time


# dependencies ------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(here)

# analysis directory
analysis_dir <- here("day1/live-demo")

# load covid-19 data ------------------------------------------------------

covid <- list(
  readr::read_csv(here(analysis_dir, "data/us-counties-2020.csv")),
  readr::read_csv(here(analysis_dir, "data/us-counties-2021.csv")),
  readr::read_csv(here(analysis_dir, "data/us-counties-2022.csv")),
  readr::read_csv(here(analysis_dir, "data/us-counties-2023.csv"))
)

# convert to 1 data frame
covid <- bind_rows(covid)


# cleaning covid data -----------------------------------------------------------

# create year_month variable
covid$year_month <- paste0(lubridate::year(covid$date), "-",
                           lubridate::month(covid$date))

# aggregate/summarize by year and month by county
covid <- covid |>
  group_by(geoid, county, state, year_month) |>
  summarize(deaths_avg_per_100k = mean(deaths_avg_per_100k, na.rm=TRUE))

# cast year_month to a factor
year_month_levels <- paste0(rep(2020:2023, each = 12), "-", rep(1:12, 4))
covid$year_month <- factor(covid$year_month, levels = year_month_levels)

# remove "USA-" from the geoids
covid$geoid <- stringr::str_remove(covid$geoid, "USA-")


# retrieve census data on poverty -----------------------------------------

popsize_and_poverty <- tidycensus::get_acs(
  year = 2020,
  geography = 'county',
  variables = c(
    popsize = "B01001_001",
    total_for_poverty = "B05010_001",
    in_poverty = "B05010_002"
  ))


# cleaning population data ------------------------------------------------

popsize_and_poverty <- popsize_and_poverty |>
  tidyr::pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate)

# create proportion in poverty estimate variable
popsize_and_poverty$proportion_in_poverty <-
  popsize_and_poverty$in_poverty / popsize_and_poverty$total_for_poverty

# drop unnecessary columns
popsize_and_poverty <- popsize_and_poverty |>
  select(GEOID, popsize, proportion_in_poverty)


# merge data together -----------------------------------------------------

covid <- left_join(covid, popsize_and_poverty, by = c('geoid' = 'GEOID'))

# create poverty levels
covid$poverty_cut <- cut(covid$proportion_in_poverty, c(0, 0.05, 0.1, 0.15, .2, 1))


# summarize covid levels by county poverty --------------------------------

covid <- covid |>
  group_by(poverty_cut, year_month) |>
  summarize(deaths_avg_per_100k =
              Hmisc::wtd.mean(deaths_avg_per_100k, normwt = popsize))


# visualize ---------------------------------------------------------------

ggplot(covid, aes(x = year_month, y = deaths_avg_per_100k,
                  color = poverty_cut, group = poverty_cut)) +
  geom_line() +
  scale_color_brewer(palette = "RdBu", direction = -1) +
  ggtitle("Avg County COVID-19 Mortality Rates by Poverty Level") +
  labs(caption = 'Data from NYTimes and tidycensus') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))
