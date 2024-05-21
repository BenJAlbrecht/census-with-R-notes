# Chapter 5
library(tidyverse)
library(tidycensus)
library(tigris)
library(patchwork)
library(mapview)
library(glue)
library(sf)


# census and acs data are associated with geographies
# geographies are repr. in the US Census Bureau's TIGER/Line database
# TIGER = Topologically Integrated Geographic Encoing and Referencing

# TIGER/Line shapefiles include 3 general types of data:
# 1) Legal entities
# 2) Statistical entities
# 3) Geographic features

# TIGRIS PACKAGE
################
# boundary of US states
st <- states()
class(st)

# visual
plot(st$geometry)

# counties for NV
nv_counties <- counties("NV")
plot(nv_counties$geometry)

# tracts for a county
wa_tracts <- tracts("NV", "Washoe")
plot(wa_tracts$geometry)

# we can also see geographic features like roads and water features
# ex, area water data
wa_water <- area_water("NV", "Washoe")
plot(wa_water$geometry)

# vector data from tigris are either points, lines, or polygons
# example of points is Census landmarks, point-of-interest dataset
# ex, landmarks in DC
dc_landmarks <- landmarks("DC", type = "point")
plot(dc_landmarks$geometry)

# lines
dc_roads <- primary_secondary_roads("DC")
plot(dc_roads$geometry)

# polygons
sf_block_groups <- block_groups("CA", "San Francisco")
plot(sf_block_groups$geometry)


# PLOTTING GEOGRAPHIC DATA
##########################
sf_tracts <- tracts("CA", "San Francisco")
ggplot(sf_tracts) +
  geom_sf() +
  theme_void()

# comparative spatial plots with a multi-plot layout
sf_block_groups <- block_groups("CA", "San Francisco")

gg1 <- ggplot(sf_tracts) +
  geom_sf() +
  theme_void() +
  labs(title = "Census tracts")

gg2 <- ggplot(sf_block_groups) +
  geom_sf() +
  theme_void() +
  labs(title = "Block groups")

gg1 + gg2

# mapview package for interactive viewing
mapview(sf_tracts)


# TIGRIS WORKFLOWS
##################

# the CB also makes cartographic boundary shapefiles
# theyre derived from the TIGER/Line but are generalized
# in the interior and clipped to the shoreline
sf_tracts_cb <- tracts("CA", "San Francisco", cb = TRUE)

sf_tiger_gg <- ggplot(sf_tracts) +
  geom_sf() +
  theme_void() +
  labs(title = "TIGER/Line")

sf_cb_gg <- ggplot(sf_tracts_cb) +
  geom_sf() +
  theme_void() +
  labs(title = "Cartographic boundary")

sf_tiger_gg + sf_cb_gg

# common issue with tigris: waiting for large file downloads
# EX: census block shapefile for 2019 Texas is 441MB
options(tigris_use_cache = TRUE)
rappdirs::user_cache_dir("tigris")

# yearly differences in TIGER/line files
# Consider ensus tracts in Tarrant COunty, Texas
# this county added nearly 1 million people 1990-2020
# thus tracts have changed!
yearly_plots <- map(seq(1990, 2020, 10), ~{
  year_tracts <- tracts("TX", "Tarrant", year = .x,
                        cb = TRUE)
  ggplot(year_tracts) +
    geom_sf() +
    theme_void() +
    labs(title = glue("{.x}: {nrow(year_tracts)} tracts"))
})
# with the plots generated, we can use patchwork to facet the plots
# the division / operator places plots on top of one another
(yearly_plots[[1]] + yearly_plots[[2]]) /
  (yearly_plots[[3]] + yearly_plots[[4]]) # tarrant county added 180 new tracts in this time!
# one method for adjusting demographic data between disparate zonal configurations
# is areal interpolation, covered later!

# combining tigris datasets:
# all US block groups for 2020
us_bgs_2020 <- block_groups(cb = TRUE, year = 2020)
nrow(us_bgs_2020)

# but this isnt an option for 2018 and earlier
# row-bind datasets
state_codes <- c(state.abb, "DC", "PR")

us_bgs_2018 <- map_dfr(
  state_codes,
  ~block_groups(
    state = .x,
    cb = TRUE,
    year = 2018
  )
)


# COORDINATE REFR. SYSTEMS
##########################
# models of geographies should represent where they are located relative
# to other locations!
# this is defined with a CRS which specifies data coord. mapping to the earth,
# and also measurements 
fl_counties <- counties("FL", cb = TRUE)
st_crs(fl_counties)
# sometimes it's hard to determine which CRS to use but
# crsuggest package can help narrow this down!
library(crsuggest)
fl_crs <- suggest_crs(fl_counties)

# Let's use the 3rd suggestion!
fl_projected <- st_transform(fl_counties, crs = 3087)

# plotting with coord_sf()
options(scipen = 999)

ggplot(fl_counties) +
  geom_sf() +
  coord_sf(crs = 3087, datum = 3087)


# WORKING W GEOMETRIES
######################
# ex: rescaling / shifting geometry for national US mapping
# good example, where do we put hawaii, puerto rico, alaska?
us_states <- states(cb = TRUE, resolution = "20m") # low res
ggplot(us_states) +
  geom_sf() +
  theme_void() # notice the Aleutian islands in far west alaska stretch this map

# we can use a projected coord reference system
# such as te continental US Albers Equal Area projection
ggplot(us_states) +
  geom_sf() +
  coord_sf(crs = 'ESRI:102003') +
  theme_void() # still not really ideal!

# tigris offers a solution here with shift_geometry()
us_states_shifted <- shift_geometry(us_states)

ggplot(us_states_shifted) +
  geom_sf() +
  theme_void() # this view uses two default args, preserve_area = FALSE
               # which shrinks alaska and inflates hawaii / PR
               # position = "below" which puts them below the US
# alternatively, we can use preserve_area = TRUE and position outside
us_states_outside <- shift_geometry(us_states,
                                    preserve_area = TRUE,
                                    position = "outside")
ggplot(us_states_outside) +
  geom_sf() +
  theme_void()

# converting polygons to points!
# ex: largest cities in the state of texas

# get city geometries
tx_places <- places("TX", cb = TRUE) %>%
  filter(NAME %in% c("Dallas", "Fort Worth", "Houston",
                     "Austin", "San Antonio", "El Paso")) %>%
  st_transform(6580)
# outline of texas
tx_outline <- states(cb = TRUE) %>%
  filter(NAME == "Texas") %>%
  st_transform(6580)

ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_places, fill = "red", color = NA) +
  theme_void() # but notice how wonky the geographies look!

# lets get the centroid of each polygon
tx_centroids <- st_centroid(tx_places)

ggplot() +
  geom_sf(data = tx_outline) +
  geom_sf(data = tx_centroids, color = "red", size = 3) +
  theme_void()
  
# exploding multipolygon geometries to single parts
# areal census features use MULTIPOLYGON type
# many shapes include disconnected areas! like islands

# Lee County, FL
lee <- fl_projected %>%
  filter(NAME == "Lee")

mapview(lee)

# explode specific parts of the multipolygon
lee_singlepart <- st_cast(lee, "POLYGON")
lee_singlepart # resulting obj has 4 rows

# sanibel island
sanibel <- lee_singlepart[2,]
mapview(sanibel)




# EXERCISES
# SAN FRAN!!!
options(scipen = 999)
sf_tracts <- tracts("CA", "San Francisco", cb = TRUE)
sf_tracts <- sf_tracts %>%
  filter(NAME != 9804.01)

sf_crs <- suggest_crs(sf_tracts)
sf_projected <- st_transform(sf_tracts, crs = 7132)

sf_map <- ggplot() +
  geom_sf(data = sf_projected, fill = "red", color = "white", size = 0.1) +
  theme_void() +
  labs(title = "San Francisco Tracts")
ggsave(
  filename = "sf_tracts2020.png",
  path = getwd(),
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)


