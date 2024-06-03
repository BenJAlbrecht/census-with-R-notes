# Reno ethnicities
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)
library(readr)

options(tigris_use_cache = TRUE)
############################################
# RENO ZCTAS ###############################
############################################

# reno zctas
reno_zctas <- zctas(
  cb = TRUE,
  starts_with = c("89"),
  year = 2018
)
reno_zctas <- reno_zctas %>%
  slice(c(170, 128, 131, 88, 137, 119, 169, 132))


# relation btw block group and zcta
zcta_relation <- read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_tabblock20_natl.txt", delim = "|")

zcta_rel <- zcta_relation %>%
  drop_na(OID_ZCTA5_20) %>%
  mutate(GEOID_ZCTA5_20 = as.numeric(GEOID_ZCTA5_20)) %>%
  filter(GEOID_ZCTA5_20 %in% reno_zctas$ZCTA5CE10) %>%
  mutate(GEOID_TABBLOCK_20 = as.numeric(GEOID_TABBLOCK_20)) %>%
  mutate(clean_geoid = str_sub(GEOID_TABBLOCK_20, start = 1, end = 11)) %>%
  mutate(clean_geoid = as.numeric(clean_geoid))

reno_geoids <- unique(zcta_rel$clean_geoid)

############################################
# RENO CENSUS ###################### TRACT #
############################################

# washoe!
washoe_race <- get_decennial(
  geography = "tract",
  state = "NV",
  county = "Washoe",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  geometry = TRUE,
) %>%
  mutate(percent = 100 * (value / summary_value))

# Clean it 
clean_washoe <- washoe_race %>%
  separate(NAME,
           into = c("tract", "county", "state"), sep = ", ") %>%
  select(-county, -state) %>%
  mutate(tract = str_replace(tract, "Census Tract ", "")) %>%
  mutate(tract = as.numeric(tract))

# Reno specific
reno <- clean_washoe %>%
  filter(GEOID %in% reno_geoids) %>%
  filter(variable %in% c("White", "Hispanic"))

# plot
reno_race_tract <- tm_shape(reno) +
  tm_facets(by = "variable", scale.factor = 4) +
  tm_borders(col = "black") +
  tm_fill(
    col = "percent", style = "jenks",
    n = 2, palette = "Blues",
    title = "Percent\n(2020 US Census)"
  ) +
  tm_layout(bg.color = "grey",
            legend.position = c(0.2, 0.75),
            panel.label.bg.color = "white")

reno_white <- reno %>% filter(variable == "White")
tm_shape(reno_white) +
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Blues",
              title = "2020 US Census",
              legend.hist = TRUE) +
  tm_layout(title = "Percent White\nby Census Tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5)

reno_hisp <- reno %>% filter(variable == "Hispanic")
tm_shape(reno_hisp) +
  tm_polygons(col = "percent",
              style = "jenks",
              palette = "Purples",
              title = "2020 US Census",
              legend.hist = TRUE) +
  tm_layout(title = "Percent Hispanic\nby Census Tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5)

