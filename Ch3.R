# Chapter 3
library(tidycensus)
library(tidyverse)

# THE TIDYVERSE
###############

# median age for all counties, 2016-2020 ACS
median_age <- get_acs(
  geography = "county",
  variables = "B01002_001",
  year = 2020
)
# youngest counties
arrange(median_age, estimate)

# oldest counties
arrange(median_age, desc(estimate))

# counties with median age of at least 50
filter(median_age, estimate >= 50)

# separate() operates on cols, other on rows
separate(
  median_age, NAME, into = c("county", "state"), sep = ", "
)

# we may want to normalize estimates
# ex: get % of a population, rather than the estimate itself
race_vars <- c(
  White = "B03002_003", Black = "B03002_004",
  Native = "B03002_005", Asian = "B03002_006",
  HIPI = "B03002_007", Hispanic = "B03002_012"
)

az_race <- get_acs(
  geography = "county", state = "AZ", variables = race_vars,
  summary_var = "B03002_001", year = 2020
)   # vars = race estimates, summary est = baseline pop

az_race_percent <- az_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)


# GROUP-WISE
############

# largest racial/ethnic group in each AZ county
largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))

# median % for each ethnic group in az counties
az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))

# HH income data for Minnesota counties, 2012-2016 ACS
mn_hh_income <- get_acs(
  geography = "county", table = "B19001", state = "MN", year = 2016
) # number of HH in each income "bin" / group.

# what if we only want 3 income categories?
# 35k > x, 75k > x > 35k, 75k < x
mn_hh_income_recode <- mn_hh_income %>%
  filter(variable != "B19001_001") %>%  # remove aggregate
  mutate( incgroup = case_when(
   variable < "B19001_008" ~ "below35k",
   variable < "B19001_013" ~ "bw35kand75k",
   TRUE ~ "above75k"
  ))

mn_group_sums <- mn_hh_income_recode %>%
  group_by(GEOID, incgroup) %>%
  summarize(estimate = sum(estimate))


# COMPARE ACS OVER TIME
#######################
# NOTE: Census API only goes back to the 2000 decennial census
# decennial census data since 1770 is available from the National Historical Geographic
# Information system (NHGIS) https://www.nhgis.org/

# caution with time series!
# big one: geography cahnges over time
# ex: Oglala Lakota County, South Dakota
oglala_lakota_age <- get_acs(
  geography = "county", state = "SD", county = "Oglala Lakota",
  table = "B01001", year = 2020
)
# how has it changed in last 10 years?
oglala_lakota_age_10 <- get_acs(
  geography = "county", state = "SD", county = "Oglala Lakota",
  table = "B01001", year = 2010
) # the county had a different name in 2010! it was Shannon county
oglala_lakota_age_10 <- get_acs(
  geography = "county",
  state = "SD",
  county = "Shannon",
  table = "B01001",
  year = 2010
)  # Note the different GEOID, when a county / entity changes the name,
   # the census bureau assigns it a new GEOID

# variable IDs can change as well.
# lets say we want to know the % of people 25 yrs and older
# with a 4 yr degree for counties in Colorado from 2019 ACS
co_college19 <- get_acs(
  geography = "county",
  variables = "DP02_0068P",
  state = "CO",
  survey = "acs1",
  year = 2019
)
# what about the same query for the 2018 1 year ACS?
co_college18 <- get_acs(
  geography = "county",
  variables = "DP02_0068P",
  state = "CO",
  survey = "acs1",
  year = 2018
) # completely different vals! clearly not percentages!
  # variable IDs are unique to each year.

# best option for time series in the ACS is Comparison Profile Tables
# available for both the 1-year and 5-year acs
# comparison profile tables also allows for more variable harmonization like
# inflation-adjusted income estimates

# inflation-adjusted median hh income for counties and county-equivalents 
# in alaska
ak_income_compare <- get_acs(
  geography = "county",
  variables = c(
    income15 = "CP03_2015_062",
    income20 = "CP03_2020_062"
  ),
  state = "AK",
  year = 2020
)

# iterating over acs years 
# vars representing estimates of pop 25+ 4-year college degree or grad degree by sex
college_vars <- c("B15002_015",
                  "B15002_016",
                  "B15002_017",
                  "B15002_018",
                  "B15002_032",
                  "B15002_033",
                  "B15002_034",
                  "B15002_035")
years <- 2010:2019   # first, a numeric vector of years
names(years) <- years
# map_dfr takes 3 args:
# first is what we iterate over, the years vector
college_by_year <- map_dfr(years, ~{
  get_acs(          # 2nd argument is a formula with the tilde ~
    geography = "county",
    variables = college_vars,
    state = "CO",
    summary_var = "B15002_001",
    survey = "acs1",
    year = .x       # the code runs for each of the years with the local var .x
  )
}, .id = "year")    # the .id arg is optional but it creates a new col that contains values equiv to the names of the input
                    # basically we make a new column for the years!
college_by_year %>% arrange(NAME, variable, year)

# now a workflow that calcs the % of college educated and makes it easier to read
percent_college_by_year <- college_by_year %>%
  group_by(NAME, year) %>%
  summarize(numerator = sum(estimate),
            denominator = first(summary_est)) %>%
  mutate(pct_college = 100 * (numerator / denominator)) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              values_from = pct_college)


# MARGINS OF ERROR
##################
# ACS estimate MOEs returned at 90% conf. level
# "We are 90% sure that the true value falls within a range defined by the est.
# plus or minues the MOE"

# median HH income by county in Rhode Island
get_acs(
  geography = "county",
  state = "Rhode Island",
  variables = "B19013_001",
  year = 2020
)
# we can change the confidence interval
get_acs(
  geography = "county",
  state = "Rhode Island",
  variables = "B19013_001",
  year = 2020,
  moe_level = 99
)

# calculating derived MOE's
# for small geo's, MOEs can be large

# age groups by sex, age 65 and older for tracts n SLC, Utah
vars <- paste0("B01001_0", c(20:25, 44:49))
salt_lake <- get_acs(
  geography = "tract",
  variables = vars,
  state = "Utah",
  county = "Salt Lake",
  year = 2020
)
# examine MOEs around estimates, lets look at one tract
example_tract <- salt_lake %>%
  filter(GEOID == "49035100100") %>%
  select(-NAME)
# in many cases, the MOEs exceed the estimates
# one soln is to aggregate data upwards until we get a good ratio of MOE  to est

# ex: ACS est of 25, with MOE of 5. the appropriate denominator for this est is 100
# with a moe of 3. to determine the moe around the derived proportion of 0.25
moe_prop(25, 100, 5, 3)


# we aggregate the data upwards to represent populations aged 65 and older by sex
salt_lake_grouped <- salt_lake %>%
  mutate(sex = case_when(
    str_sub(variable, start = -2) < "26" ~ "Male",
    TRUE ~ "Female"
  )) %>%
  group_by(GEOID, sex) %>%
  summarize(sum_est = sum(estimate),
            sum_moe = moe_sum(moe, estimate))


# EXERCISES
###########
# percent of pop age 25 and up with bachelor's degree
nv_educ <- get_acs(
  geography = "county",
  variables = "DP02_0068P",
  state = "Nevada",
  year = 2019
)
high_county <- nv_educ %>%
  filter(estimate == max(estimate))
low_county <- nv_educ %>%
  filter(estimate == min(estimate))
median_educ <- nv_educ %>%
  summarise(median = median(estimate))













