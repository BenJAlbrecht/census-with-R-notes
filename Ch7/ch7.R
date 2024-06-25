# chapter 7
# spatial analysis
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(magrittr)
library(scales)
options(tigris_use_cache = TRUE)

# Spatial overlay
#################
# spatial analysis can allow us to see how geo data interrelate in space

# note: aligning coordinate reference systems
# in sect 5.4 we covered coordinate reference systems in R
# how important they are, how to select projected coord systems
# using crsuggest

# spatial data with tigris or tidycensus will by default share the CRS
# NAD 1983
# sf uses the s2 library to compute 3-dimensional overlay rather than
# assuming planar geometries

# recommended spatial analysis data prep workflow:
# 1) download data
# 2) use suggest_crs()
# 3) transform data to projected CRS using st_transform()
# 4) compute spatial overlay 

# ex: what geographies are wtihin a metro area?

# ex, kansas city
# has tracts in kansas and missouri!

ks_mo_tracts <- map_dfr(c("KS", "MO"), ~{
  tracts(.x, cb = TRUE, year = 2020)
}) %>%
  st_transform(8528)

kc_metro <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Kansas City")) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = ks_mo_tracts, fill = "white", color = "grey") +
  geom_sf(data = kc_metro, fill = NA, color = "red") +
  theme_void()

# do with reno/sf/sd?
metros <- core_based_statistical_areas(cb = TRUE, year = 2020)
view(metros)

# reno #
reno_metro <- metros %>%
  filter(str_detect(NAME, "Reno")) %>%
  st_transform(8528)

reno_tracts <- tracts(
  state = "NV",
  cb = TRUE,
  year = 2020
) %>%
  st_transform(8528)

# reno plot
ggplot() +
  geom_sf(data = reno_tracts, fill = "white", color = "grey") +
  geom_sf(data = reno_metro, fill = NA, color = "red") +
  theme_void()


# sf #
######
sf_metro <- metros %>%
  filter(str_detect(NAME, "San Francisco")) %>%
  st_transform(8528)

sf_tracts <- tracts(
  state = "CA",
  cb = TRUE, year = 2020
) %>%
  st_transform(8528)

# sf plot
ggplot() +
  geom_sf(data = sf_tracts, fill = 'white', color = 'grey') +
  geom_sf(data = sf_metro, fill = NA, color = "red") +
  theme_void()

# sd #
######
sd_metro <- metros %>%
  filter(str_detect(NAME, "San Diego")) %>%
  st_transform(8528)

sd_tracts <- tracts(
  state = "CA", cb = TRUE, year = 2020
) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = sd_tracts, fill = "white", color = "grey") +
  geom_sf(data = sd_metro, fill = NA, color = "red") +
  theme_void()

# same but with urban areas: ? #
################################
urbans <- urban_areas()
# reno
reno_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "Reno, NV")) %>%
  st_transform(8528)

reno_tracts <- tracts(
  state = "NV",
  county = "Washoe",
  cb = TRUE,
  year = 2020
) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = reno_tracts, fill = 'white', color = 'grey') +
  geom_sf(data = reno_urb, fill = NA, color = "red")

# sf
sf_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "San Francisco")) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = sf_tracts, fill = "white", color = "grey") +
  geom_sf(data = sf_urb, fill = NA, color = "red")

# sd
sd_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "San Diego")) %>%
  st_transform(8528)

ggplot() +
  geom_sf(data = sd_tracts, fill = "white", color = 'grey') +
  geom_sf(data = sd_urb, fill = NA, color = "red") +
  theme_void()


# spatial subsetting in sf package
ca_tracts <- tracts(
  state = "CA", cb = TRUE, year = 2020
) %>%
  st_transform(8528)

sd_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "San Diego")) %>%
  st_transform(8528)

sd_tracts <- st_intersection(ca_tracts, sd_urb)

bb_tracts <- st_bbox(sd_tracts)
bb_tracts[1]

# Manually define bounding box coordinates for San Diego
xmin <- bb_tracts[1]  # replace with the appropriate value
xmax <- bb_tracts[3]  # replace with the appropriate value
ymin <- bb_tracts[2]   # replace with the appropriate value
ymax <- bb_tracts[4]   # replace with the appropriate value

# Plot the data with manual zoom
ggplot() +
  geom_sf(data = sd_tracts, fill = "white", color = "grey") +
#  geom_sf(data = sd_urb, fill = NA, color = "red") +
  theme_void() +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

# book ex
kc_tracts <- ks_mo_tracts[kc_metro, ]

ggplot() + 
  geom_sf(data = kc_tracts, fill = "white", color = "grey") + 
  geom_sf(data = kc_metro, fill = NA, color = "red") + 
  theme_void()

# note that this still yields tracts within, that cross or touch the boundary
# this may be ineffective for us who want stats for tracts only within the metro
reno_tracts_within <- reno_tracts %>%
  st_filter(reno_urb, .predicate = st_within)

# equivalent syntax:
# reno_urb2 <- reno_tracts[reno_urb, op = st_within]

ggplot() +
  geom_sf(data = reno_tracts_within, fill = "white", color = "black") +
  geom_sf(data = reno_urb, fill = NA, color = "red") +
  theme_void()


# Spatial joins
###############
# a spatial join is like transferring attributes between spatial layers
# its like a table join!

# point-in-polygon spatial joins
# table of coords is matched to census polygons to determine
# demographic characteristics around those locations

# ex: health analyst in gainesville, florida
# needs to determine the % of residents age 65 and up who 
# lack health insurance in patients neighborhoods
gainesville_patients <- tibble(
  patient_id = 1:10,
  longitude = c(-82.308131, -82.311972, -82.361748, -82.374377, 
                -82.38177, -82.259461, -82.367436, -82.404031, 
                -82.43289, -82.461844),
  latitude = c(29.645933, 29.655195, 29.621759, 29.653576, 
               29.677201, 29.674923, 29.71099, 29.711587, 
               29.648227, 29.624037)
)


gainesville_sf <- gainesville_patients %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(6440)

# map data
mapview(
  gainesville_sf,
  col.regions = 'red',
  legend = FALSE
)

# now that we have patient data, we need data on insurance from the ACS
alachua_insurance <- get_acs(
  geography = "tract",
  variables = "DP03_0096P",
  state = "FL",
  county = "Alachua",
  year = 2019,
  geometry = TRUE
) %>%
  select(GEOID, pct_insured = estimate,
         pct_insured_moe = moe) %>%
  st_transform(6440)

# before computing the spatial join, the spatial relationships
# between patient points and tract demographics can be visualized
mapview(
  alachua_insurance,
  zcol = "pct_insured",
  layer.name = "% with health<br/>insurance"
) +
  mapview(
    gainesville_sf,
    col.regions = "red",
    legend = FALSE
  )

# lets formalize this with a spatial join!
patients_joined <- st_join(
  gainesville_sf,
  alachua_insurance
)


# spatial joins & group-wise spatial analysis
# ex: distributions of neighborhoods (census tracts)
#     by hispanic pop for the 4 largest metros in TX

tx_cbsa <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE
) %>%
  filter(str_detect(NAME, "TX")) %>%
  slice_max(estimate, n = 4) %>%
  st_transform(6579)

# % hisp by tract
pct_hispanic <- get_acs(
  geography = "tract",
  variables = "DP05_0071P",
  state = "TX",
  year = 2019,
  geometry = TRUE
) %>%
  st_transform(6579)

# we only want tracts within our metros, we can do this with a spatial join!!!
hispanic_by_metro <- st_join(
  pct_hispanic,
  tx_cbsa,
  join = st_within,
  suffix = c("_tracts", "_metro"),
  left = FALSE
)

# group-wise data vis across metro areas with faceted plot:
hispanic_by_metro %>%
  mutate(NAME_metro = str_replace(NAME_metro, ", TX Metro Area", "")) %>%
  ggplot() +
  geom_density(aes(x = estimate_tracts),
               color = "navy", fill = "navy",
               alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~NAME_metro) +
  labs(
    title = "Distribution of Hispanic/Latino population by Census tract",
    subtitle = "Largest metropolitan areas in Texas",
    y = "Kernel density estimate",
    x = "Percent Hispanic/Latino in Census tract"
  )

# output from a spatial join operation can also be "rolled up" to larger
# geos, through group-wise analysis
# say we want to know the median value of these distributions
median_by_metro <- hispanic_by_metro %>%
  group_by(NAME_metro) %>%
  summarize(median_hispanic = median(estimate_tracts, na.rm = TRUE))


# it also summarizes geometry by groups!
plot(median_by_metro[1,]$geometry)


# small area time-series analysis
#################################
# time-series analysis for census data only really works with large areas
# like counties, which dont change over time

# smaller geos get re-drawn all the time!

# we can solve this with __areal interpolation__

# areal interpolation == alllocation of data from one set of zones to a second
# overlapping set of zones that may or may not perfectly align spatially

# two such approaches: 1) area-weighted interpolation
#                      2) population-weighted interpolation

# ex: data for maricopa county, AZ
#     no. of people working from home
# delta between 2011-2015 ACS (2010 boundaries), 2016-2020 ACS (2020 bounds)

wfh_15 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2015,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  select(estimate) %>%
  st_transform(26949)

wfh_20 <- get_acs(
  geography = "tract",
  variables = "B08006_017",
  year = 2020,
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
) %>%
  st_transform(26949)

# area-weighted areal interpolation
# uses the area of overlap of geos as the interpolation weights
# an intersection is computed between origin-destination
# weights are computed as the proportion of the overall origin area

wfh_interpolate_aw <- st_interpolate_aw( # aw = area weighted
  wfh_15, wfh_20,
  extensive = TRUE # extensive = TRUE <- weighted sums, if false <- weighted means
) %>%
  mutate(GEOID = wfh_20$GEOID)

# note the error we get
# st_interpolate_aw assumes attributes are constant or uniform over areas of x
# assumption that larger areas have more people is often incorrect
# alt method: pop-weighted!
maricopa_blocks <- blocks(
  state = "AZ",
  county = "Maricopa",
  year = 2020
)

wfh_interpolate_pw <- interpolate_pw(
  wfh_15, wfh_20,
  to_id = "GEOID",
  extensive = TRUE,
  weights = maricopa_blocks,
  weight_column = "POP20",
  crs = 26949
)

# small-area comparisons!
# now let's join the pop-weighted interpolated 2011-2015 data
# to the original 2016-2020 with a left_join
wfh_shift <- wfh_20 %>%
  left_join(st_drop_geometry(wfh_interpolate_pw),
            by = "GEOID",
            suffix = c("_2020", "_2015")) %>%
  mutate(wfh_shift = estimate_2020 - estimate_2015)

ggplot() +
  geom_sf(data = wfh_shift, aes(fill = wfh_shift), color = NA,
          alpha = 0.8) +
  scale_fill_distiller(palette = "PuOr", direction = -1) +
  labs(
    fill = "Shift, 2011-2015 to\n2016-2020 ACS",
    title = "Change in work-from-home population",
    subtitle = "Maricopa County, Arizona"
  ) +
  theme_void()


# Distance and proximity analysis
#################################
# most simple method: straight-line (Euclidean) distances
# complex/more accurate: transportation networks
#                        travel times such as walking, cycling or driving

# ex: accessibility to level 1 and level 2 trauma hospitals by census tract in iowa

# CRS: NAD83 / Iowa North
ia_tracts <- tracts(
  "IA", cb = TRUE, year = 2019
) %>% st_transform(26975)

hospital_url <- "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/Hospital/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

trauma <- st_read(hospital_url) %>%
  filter(str_detect(TRAUMA, "LEVEL I\\b|LEVEL II\\b|RTH|RTC")) %>%
  st_transform(26975) %>%
  distinct(ID, .keep_all = TRUE)

# to see distance we need to identify not only those hosptials in iowa,
# but also those in other states near the iowa border such as in omaha, NE
# and rock island, IL
# lets apply a distance threshold in st_filter()
ia_trauma <- trauma %>%
  st_filter(ia_tracts,
            .predicate = st_is_within_distance,
            dist = 100000)

ggplot() +
  geom_sf(data = ia_tracts, color = "NA", fill = "grey50") +
  geom_sf(data = ia_trauma, color = "red") +
  theme_void()

# census tract & hospital data in hand
# we now calculate distances from tracts to trauma centers using the st_distance()
# we calc distances from the centroids of iowa census tracts to each trauma center
dist <- ia_tracts %>%
  st_centroid() %>%
  st_distance(ia_trauma)

dist[1:5, 1:5]

min_dist <- dist %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

hist(min_dist)

# travel time needs mapbox api :(


# better catrography with spatial overlay
#########################################
# the census cartographic boundaries include water areas! how do we toss these?

# median HH inc by tract in manhattan
ny <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  year = 2020,
  geometry = TRUE
)

ggplot(ny) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(
    fill = "Median household\nincome"
  )

# note theres water!
# we can use the core TIGER/Line shapefiles instead
# then erase water area!
ny2 <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = "New York",
  geometry = TRUE,
  year = 2020,
  cb = FALSE # get TIGER/Line instead of cartographic boundary shapefiles
) %>%
  st_transform(6538)

# erase water boundaries
ny_erase <- erase_water(ny2)
ggplot(ny_erase) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median household\nincome")


# milwaukee for dad?
library(crsuggest)

mke <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "WI",
  county = "Milwaukee",
  geometry = TRUE,
  year = 2020,
  cb = FALSE
) %>%
  st_transform(6538) %>%
  separate(NAME, into = c("tract", "county", "state"), sep = ", ") %>%
  mutate(tract = str_replace(tract, "Census Tract ", "")) %>%
  filter(tract != 9900)

mke %>% filter(tract == 9900)
mke_erase <- erase_water(mke, area_threshold = 0.25)

mke_erase %>% filter(estimate == 0)

ggplot(mke_erase) +
  geom_sf(aes(fill = estimate)) +
  scale_fill_distiller(palette = "YlGnBu", labels = scales::label_dollar()) +
  theme_void() +
  labs(
    fill = "Median household\nincome",
    title = "Median household income by Census tract",
    subtitle = "Milwaukee County, Wisconsin",
    caption = "Source: Census Bureau ACS 5-year, 2016-2020"
  )


# spatial neighborhoods & spatial weights matrices
##################################################
# the spatial capabilities of tidycensus allow us to do eda with spatial data
# (ESDA). We may explore patterns or identify clusters of attributes in datasets.

# to illustrate this, lets look at median age by census tract in Dallas-Fort Worth TX metro area
library(spdep)

# CRS: NAD83 / Texas North Central
dfw <- core_based_statistical_areas(cb = TRUE, year = 2020) %>%
  filter(str_detect(NAME, "Dallas")) %>%
  st_transform(32138)

dfw_tracts <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(32138) %>%
  st_filter(dfw, .predicate = st_within) %>%
  na.omit()

ggplot(dfw_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  theme_void()

# ESDA relies on the concept of a *neighborhood* which is a repr. of a 
# geographic feature and how it relates with other features nearby
# spdep supports a variety of neighborhood definitions:
# 1) proximity-based, features are identified based on some measure of distance
# 2) graph-based, define through network relationships (ex: along a street)
# 3) contiguity-based:
#                     i) queen's case: all polygons that share at least one vertex
#                      ii) rook's case, polygons must share at least one line segment

# in this example we'll use a queen's case contiguity-based neighborhood defn
neighbors <- poly2nb(dfw_tracts, queen = TRUE)

summary(neighbors)

# visualize neighborhood relationships 
dfw_coords <- dfw_tracts %>%
  st_centroid() %>%
  st_coordinates()

plot(dfw_tracts$geometry)
plot(neighbors,
     coords = dfw_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)

# row indices for the neighbors of a given feature
# ex: row indices of the neighbors of the census tract at row index 1
neighbors[[1]]

# generating the spatial weights matrix
weights <- nb2listw(neighbors, style = "W")
weights$weights[[1]]


# global & local spatial autocorrelation
########################################
# spatial autocorrelation == how attributes of geographic features relate to 
#                            those of their neighbors.

# relating to Waldo Tobler's famous "first law of geography"
#         "everything is related to everything else,
#                        but near things are more related than distant things."
# we may be interested in the degree to which a given census variable clusters
# spatially and subsequently where those clusters are found!

# one way to assess clustering is to assess the degree to which ACS estimates
# are similar to or differ from those of their neighbors. patterns can be explained
# as follows...
# 1) spatial clustering -- data tend to be similar to neighboring data
# 2) spatial uniformity -- data tend to differ from neighboring data values
# 3) spatial randomness -- there is no apparent relationship!

# Spatial lags & moran's I
# spatial weight matrices can be used for spatial lags
# a spatial lag refer to the neighboring values of an observation
# row-standardized weight matrices will make lagged means and binary
# weight matrices will produce lagged sums
dfw_tracts$lag_estimate <- lag.listw(weights, dfw_tracts$estimate)
# this makes a new column in dfw_tracts that represents the average
# median age for the neighbors of each census tract in the DFW metro area!

# we can draw a scatter of the ACS estimte vs its lagged mean, to do a preliminary
# analysis of spatial clustering in the data
ggplot(dfw_tracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Median age by Census tract, Dallas-Fort Worth TX",
    x = "Median age",
    y = "Spatial lag, median age",
    caption = "Data source: 2016-2020 ACS via the tidycensus R package\n
    Spatial relationships based on queens-case polygon contiguity."
  )

# we can further the test by looking at global spatial autocorrelation
# the most common method used for this is Moran's I
# it's like a correlation coefficient but for the relation between obs and
# their neighbors
moran.test(dfw_tracts$estimate, weights)


# local G 
# for Gi*
# For Gi*, re-compute the weights with `include.self()`
localg_weights <- nb2listw(include.self(neighbors))

dfw_tracts$localG <- localG(dfw_tracts$estimate, localg_weights)
dfw_tracts$localG <- as.numeric(dfw_tracts$localG)
ggplot(dfw_tracts) + 
  geom_sf(aes(fill = localG), color = NA) + 
  scale_fill_distiller(palette = "RdYlBu") + 
  theme_void() + 
  labs(fill = "Local Gi* statistic")

# Given that the returned results are Z-scores, we can choose hot spot
# thresholds in the statistic!
dfw_tracts <- dfw_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "High cluster",
    localG <= -2.576 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

ggplot(dfw_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("red", "blue", "grey")) +
  theme_void()


# Do this with reno?
reno_urb <- urbans %>%
  filter(str_detect(NAMELSAD10, "Reno, NV")) %>%
  st_transform(8528)

reno_tracts <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = "NV",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(8528) %>%
  st_filter(reno_urb, .predicate = st_within) %>%
  na.omit()

ggplot(reno_tracts) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c() +
  geom_sf(data = reno_urb, fill = NA, color = "red") +
  theme_void()

neighbors_reno <- poly2nb(reno_tracts, queen = TRUE)
reno_coords <- reno_tracts %>%
  st_centroid() %>%
  st_coordinates()

plot(reno_tracts$geometry)
plot(neighbors_reno,
     coords = reno_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)

reno_weights <- nb2listw(neighbors_reno, style = "W")

reno_tracts$lag_estimate <- lag.listw(reno_weights, reno_tracts$estimate)

ggplot(reno_tracts, aes(x = estimate, y = lag_estimate)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() 

moran.test(reno_tracts$estimate, reno_weights)
  
localg_weights_reno <- nb2listw(include.self(neighbors_reno))  

reno_tracts$localG <- localG(reno_tracts$estimate, localg_weights_reno)
reno_tracts$localG <- as.numeric(reno_tracts$localG)

ggplot(reno_tracts) +
  geom_sf(aes(fill = localG), color = NA) +
  scale_fill_distiller(palette = "RdYlBu") +
  theme_void()
  
reno_tracts <- reno_tracts %>%
  mutate(hotspot = case_when(
    localG >= 2.576 ~ "Older",
    localG <= -2.576 ~ "Younger",
    TRUE ~ "Not significant"
  ))

ggplot(reno_tracts) +
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) +
  scale_fill_manual(values = c("grey", "red", "blue")) +
  geom_sf(data = reno_urb, fill = NA, color = "black") +
  theme_void() +
  labs(
    title = "Age-group clusters by Census tract, Reno-Sparks NV",
    fill = "Clusters",
    caption = "Data source: 2016-2020 ACS\n
               Spatial relationships based on local spatial autocorrelation"
  )

# another method of identifying clusters and spatial outliers!
# with local indicators of spatial association (LISA)
# (local form of Moran's I)

set.seed(1983)

dfw_tracts$scaled_estimate <- as.numeric(scale(dfw_tracts$estimate))

dfw_lisa <- localmoran_perm(
  dfw_tracts$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("local_i", "exp_i", "var_i", "z_i", "p_i",
              "p_i_sim", "pi_sim_folded", "skewness", "kurtosis"))

dfw_lisa_df <- dfw_tracts %>%
  select(GEOID, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(dfw_lisa)

dfw_lisa_clusters <- dfw_lisa_df %>%
  mutate(lisa_cluster = case_when(
    p_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & local_i > 0 ~ "High-high",
    scaled_estimate > 0 & local_i < 0 ~ "High-low",
    scaled_estimate < 0 & local_i > 0 ~ "Low-low",
    scaled_estimate < 0 & local_i < 0 ~ "Low-high"
  ))


color_values <- c(`High-high` = "red", 
                  `High-low` = "pink", 
                  `Low-low` = "blue", 
                  `Low-high` = "lightblue", 
                  `Not significant` = "white")

ggplot(dfw_lisa_clusters, aes(x = scaled_estimate, 
                              y = lagged_estimate,
                              fill = lisa_cluster)) + 
  geom_point(color = "black", shape = 21, size = 2) + 
  theme_minimal() + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_fill_manual(values = color_values) + 
  labs(x = "Median age (z-score)",
       y = "Spatial lag of median age (z-score)",
       fill = "Cluster type")


ggplot(dfw_lisa_clusters, aes(fill = lisa_cluster)) + 
  geom_sf(size = 0.1) + 
  theme_void() + 
  scale_fill_manual(values = color_values) + 
  labs(fill = "Cluster type")







