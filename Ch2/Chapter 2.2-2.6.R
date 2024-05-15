# 2.2, 2.4, 2.5 geography and variables in tidycensus
library(tidyverse)
library(tidycensus)

# GEOGRAPHY & VARIABLES
#######################

# cbsa = "core-based statistical areas"
# term for metropolitan and micropolitan statistical areas
cbsa_population <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2020
)

# geographic subsets
# household income in wisconsin counties
wi_income <- get_acs(
  geography = "county", variables = "B19013_001",
  state = "WI", year = 2020      # tidcensus accepts state names, postal codes, state FIPS
)
wi_income <- wi_income %>% arrange (desc(estimate))

# smaller geographies like census tracts
# tracts fit neatly within counties!
# Dane County income (Madison!)
dane_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WI",
  county = "Dane", year = 2020
)

# Note 5-year ACS covers geographies down to block
# but 1-year ACS only covers geographies of pop >= 65k
# nation > region > division > state > counties > tracts > block group > blocks
nrow(wi_income) # gives 72 observations
wi_income_1yr <- get_acs(
  geography = "county", variables = "B19013_001",
  state = "WI", year = 2019, survey = "acs1"
)
nrow(wi_income_1yr) # only 23


# SEARCHING FOR VARIABLES
#########################
v16 <- load_variables(2016, "acs5", cache = TRUE)


# DATA STRUCTURE
################
hhinc <- get_acs(
  geography = "state", table = "B19001",
  survey = "acs1", year = 2016
)  # each row represents a state-characteristic combination

hhinc_wide <- get_acs(
  geography = "state", table = "B19001",
  survey = "acs1", year = 2016, output = "wide"
) # alternatively, output = "wide" gives variables spread across the columns
# note that variables with E at the end are estimates, M margin of error

# understanding GEOIDs
# GEOID uniquely identifies geographic units
# EX: households by census block in Cimarron County, OK
cimarron_blocks <- get_decennial(
  geography = "block",
  variables = "H1_001N",
  state = "OK",
  county = "Cimarron",
  year = 2020,
  sumfile = "pl"
)

# renaming variable IDs
# median hh income and median age
ga <- get_acs(
  geography = "county", state = "Georgia",
  variables = c(medinc = "B19013_001", medage = "B01002_001"),
  output = "wide", year = 2020
)


# OTHER CENSUS DATASETS
#######################
# see censusapi package for missing datasets

# Population Estimates Program (PEP)
# Yearly estimates of US pop (uses projections not survey)
queens_components <- get_estimates(
  geography = "county", product = "components",
  state = "NY", county = "Queens", year = 2019
)

# product = characteristics, using breakdown arg, gets breakdown of pop
# for states and counties.
# To get pop estimates by sex and hispanic origin for Louisiana
louisiana_sex_hisp <- get_estimates(
  geography = "state", product = "characteristics", breakdown = c("SEX", "HISP"),
  breakdown_labels = TRUE, state = "LA", year = 2019
)

# we can also get flows
# from ACS migration flows API
# uses 5-year ACS sample
honolulu_migration <- get_flows(
  geography = "county",
  state = "HI", county = "Honolulu", year = 2019
)


# DEBUGGING
###########

# tidycensus carries through the error message from the Census API
# or translates common errors. In this example, a user has mis-typed
# the variable ID
state_pop <- get_decennial(
  geography = "state", variables = "P01001", year = 2010
)

# error for geography that isnt available
cbsa_ohio <- get_acs(
  geography = "cbsa", variables = "DP02_0068P",
  state = "OH", year = 2019
)

# to assist with debugging, tidycensus offers a parameter
# show_call that prints out the actual API call that tidycensus
# makes to the Census API
cbsa_bachelors <- get_acs(
  geography = "cbsa", variables = "DP02_0068P",
  year = 2019, show_call = TRUE
)


# EXERCISES
###########
# 1)
nv_age <- get_acs(
  geography = "county", variables = "B01002_001",
  state = "NV", year = 2020
)
nv_age <- nv_age %>% arrange(desc(estimate))

# 2)
nv20 <- load_variables(2016, "acs5", cache = TRUE)
