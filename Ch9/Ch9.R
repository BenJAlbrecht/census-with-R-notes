# Ch 9
library(tidyverse)
library(tidycensus)
library(tigris)

options(tigris_use_cache = TRUE)

# What is microdata ------------------------------------------------------------
#-------------------------------------------------------------------------------
# Microdata refer to individual-level data. In other words, anonymized individual
# survey responses. US census microdata are available for both the decennial
# and ACS. These datasets named the Public use microdata series PUMS.

# ACS PUMS are available in 1 and 5 year versions. The 1-year covers about 1%
# of the US pop. Whereas the 5 year PUMS covers about 5%
# So microdata represents a smaller subset of the US pop than regular ACS!

# The Census bureau also has a network of Federal Statistical Research Data
# Centers (FSRDCs) that grant access to microdata with larger samples and
# more detail; but your research needs approval to use these.

# IPUMS
# One of the most popular and comprehensive databases for microdata is the
# Univerity of Minnesota's IPUMS project.
# IPUMS contains decennial, ACS, CPS, and over 100 countries.

# IPUMS are harmonized, so the changing variable definitiosn over time are
# aligned for longitudinal analysis.
#-------------------------------------------------------------------------------


# Using microdata in tidycensus ------------------------------------------------
#-------------------------------------------------------------------------------
# microdata for Wyoming for the 1 year 2019 ACS with info on sex, age, and
# household type
wy_pums <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019
)

# We can interpret the weights as
# "the number of observations in the general population represented by this
#  particular row in the dataset"
# so if PWGTP is 50, then about 50 people in Wyoming have the demographics
# of the person in that row

# Inferences about pop characteristics can be made by summing over the weights
# lets say we want an estimate of the number of people in Wyoming who are 50
# years or older in 2019 and compare this with the total pop in WY

wy_age_50 <- wy_pums %>%
  filter(AGEP == 50)

sum(wy_pums$PWGTP) # 578,759 total people
sum(wy_age_50$PWGTP) # 4,756 over the age of 50
# of course the estimates have a MOE, which we will cover next chapter!

# important to note that get_pums() returns two separate weights cols
# one for HH and one for persons.
# Lets take a look at a single household in the Wyoming dataset
wy_hh_ex <- wy_pums %>%
  filter(SERIALNO == "2019HU0456721")
# This HH has:
# a woman aged 40,
# a man aged 45,
# a girl aged 8,
# and a boy aged 5
# The HH weigth value WGTP is identical for all HH members, whereas the person
# weight val PWGTP is not.
# Also note the HHT value is 1, which tells us that this is a married-couple
# household!

# We need to take care to use appropriate weights and filters
# for HH level or person level analyses

# For ex, to find the number of HH in wyoming the data should be filtered to
# records where the SPORDER is 1 then summed over the WGTP column.
# Persons in group quarters will be excluded automatically as they have a HH
# weight of 0
wy_hhs <- wy_pums %>%
  filter(SPORDER == 1)

sum(wy_hhs$WGTP)

# Housing units rather than simply household-level analyses introduce
# another level here. Cuz they can be occupied or vacant!

# Vacant housing units are returned in a different format from the Census API
# which makes them a special case
wy_with_vacant <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019,
  return_vacant = TRUE
) %>%
  arrange(VACS)
#-------------------------------------------------------------------------------


# Working with PUMS variables --------------------------------------------------
#-------------------------------------------------------------------------------
# like other data dictionaries we can browse the PUMS data dictionary
view(pums_variables)

# Typical workflow involves browsing the appropriate data dictionary
# choosing var IDs and using those in scripts
# lets see wy data with more details requested
wy_pums_recoded <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs1",
  year = 2019,
  return_vacant = FALSE,
  recode = TRUE
)

# Datasets can be huge. So we can filter the data down so our internet
# has a better time

# EX: WY request to return only women between the ages of 30 and 49
# but this time from the 5 year ACS PUMS
wy_pums_filtered <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "WY",
  survey = "acs5",
  variables_filter = list(
    SEX = 2,
    AGEP = 30:49
  ),
  year = 2019,
  recode = TRUE
)
#-------------------------------------------------------------------------------


# Public Use Microdata Areas (PUMAs) -------------------------------------------
#-------------------------------------------------------------------------------
# PUMAs are the smallest available geographies that records are identifiable in
# the PUMS datasets
# PUMAs are redrawn with each decennial census
# usually they have 100k - 200k people

wy_pumas <- pumas(
  state = "WY",
  cb = TRUE,
  year = 2019
)

ggplot(wy_pumas) +
  geom_sf() +
  theme_void()

# the object from pumas has labels with descriptions of these areas
wy_pumas$NAME10

# in denser urban areas, PUMAs will reflect subsections of major cities
# and are drawn in attempts to reflect meaningful local areas

# in NYC for ex, PUMAs are drawn to align with recognized community districts
nyc_pumas <- pumas(
  state = "NY",
  cb = TRUE,
  year = 2019
) %>%
  filter(str_detect(NAME10, "NYC"))

ggplot(nyc_pumas)

# Working with PUMAs in PUMS data
# PUMA info is available with the variable code PUMA in get_pums()
wy_age_by_puma <- get_pums(
  variables = c("PUMA", "AGEP"),
  state = "WY",
  survey = "acs5",
  year = 2019
)
# PUMA IDs are replicated across states so we should also use the state fips 

# we can also use puma to get data for a specific or multiple pumas
wy_puma_subset <- get_pums(
  variables = "AGEP",
  state = "WY",
  survey = "acs5",
  puma = "00500",
  year = 2019
)

# again since PUMA ids are the same across states if we want multi-state
# geos we need to adapt this slightly
twostate_puma_subset <- get_pums(
  variables = "AGEP",
  state = "multiple",
  survey = "acs5",
  puma = c("WY" = "00500",
           "UT" = "05001"),
  year = 2019
)
#-------------------------------------------------------------------------------


# Exercises --------------------------------------------------------------------
#-------------------------------------------------------------------------------
nv_pums <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "NV",
  survey = "acs1",
  variables_filter = list(
    AGEP = 24:28
  ),
  year = 2022,
  recode = TRUE
)

# Find women between ages of 24 - 28
nv_w <- nv_pums %>%
  filter(SEX == 2)

# Men
nv_m <- nv_pums %>%
  filter(SEX == 1)

sum(nv_w$PWGTP) # 103,233 women between ages of 24 - 28
sum(nv_m$PWGTP) # 109,732 men between ages of 24 - 28

# nv pumas
nv_pumas <- pumas(
  state = "NV",
  cb = TRUE,
  year = 2020
)

ggplot(nv_pumas) +
  geom_sf() +
  theme_void()

# sf pumas
sf_pumas <- pumas(
  state = "CA",
  cb = TRUE,
  year = 2020
) %>%
  filter(str_detect(NAMELSAD20, "San Francisco"))

ggplot(sf_pumas) +
  geom_sf() +
  theme_void()

# Degrees in nevada
nv_pums_educ <- get_pums(
  variables = c("FOD1P"),
  state = "NV",
  survey = "acs1",
  year = 2022,
  recode = TRUE
) %>%
  filter(!is.na(FOD1P_label))
  
degree_counts <- nv_pums_educ %>%
  group_by(FOD1P_label) %>%
  summarize(degree_holders = sum(PWGTP)) %>%
  arrange(desc(degree_holders))

view(degree_counts)




























