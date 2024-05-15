# 2.1 intro to tidycensus
library(tidycensus)
library(tidyverse)

# census_api_key("your_key", install = TRUE)


# DECENNIAL CENSUS
##################
total_population_10 <- get_decennial(
  geography = "state", variables = "P001001",
  year = 2010
)

# Percent of aian by state
pop20 <- get_decennial(
  geography = "state", variables = "P1_001N",
  year = 2020, sumfile = "pl"
)
aian20 <- get_decennial(
  geography = "state", variables = "P1_005N",
  year = 2020, sumfile = "pl"
)

data <- left_join(pop20, aian20, by = "NAME")
data <- data %>%
  mutate(aian_p = round(value.y / value.x, 4) * 100) %>%
  select(NAME, aian_p) %>%
  arrange(desc(aian_p))


# ACS
#####
born_in_mexico <- get_acs(
  geography = "state", variables = "B05006_150", year = 2020
) # If we don't specify year, it goes to most recent 5 year
  # if we don't specify survey, it goes to 5-year ACS

born_in_mexico_1yr <- get_acs(
  geography = "state", variables = "B05006_150", survey = "acs1", year = 2019
) # notice the NA for geographies with smaller populations

# We can also grab whole tables instead of specific variables.
# This table is sex broken down by age.
age_table <- get_acs(
  geography = "state", table = "B01001", year = 2020
)


getwd()