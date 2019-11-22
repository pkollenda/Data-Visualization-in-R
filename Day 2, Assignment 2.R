# Search for Questions with Strg + F "!!!".

"
Project:  Data Visualizations in R
Author:   Philipp Kollenda

Script:   Day 2, Assignment 2
"

# --- Step 0: Settings -----------------------------------------------------------------------------
# R Options
rm(list = ls())
options(scipen = 10, stringsAsFactors = FALSE)

# Selective Execution

# Decisions

# Libraries
library(survey)
library(srvyr)
library(tidyverse)
library(socviz)

options(survey.lonely.psu = "adjust"); options(na.action="na.pass")

# --- Question 1: Survey Data  ---------------------------------------------------------------------
gss_wt <- subset(gss_lon, year > 1974) %>% 
  mutate(stratvar = interaction(year, vstrat)) %>% 
  as_survey_design(ids = vpsu, strata = stratvar, weights = wtssall, nest = TRUE)

out_grp <- gss_wt %>% filter(year %in% seq(1976, 2016, by = 4)) %>% group_by(year, race, degree) %>% 
  summarize(prop = survey_mean(na.rm = TRUE))

out_mrg <- gss_wt %>% filter(year %in% seq(1976, 2016, by = 4)) %>% 
  mutate(racedeg = interaction(race, degree)) %>% group_by(year, racedeg) %>% 
  summarize(prop = survey_mean(na.rm = TRUE)) %>% 
  separate(racedeg, sep = "\\.", into = c("race", "degree"))

# Preparation for Figures
out_grp <- out_grp %>% filter(race %in% c("White", "Black")) %>% 
  filter(!is.na(degree))

p_main <- ggplot(data = out_grp, aes(y = prop, group = race, color = race, fill = race)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels = scales::wrap_format(width = 8)) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(x = element_blank(), y = "Percent", title = "Educational Attainment by Race", subtitle = "GSS 1976-2016", color = "Race", fill = "Race") +
  theme_bw()

# Figure 1
p_main + 
  geom_col(aes(x = degree), position = position_dodge(), alpha = 0.2) + 
  geom_errorbar(aes(x = degree, ymin = prop - 1.96 * prop_se, ymax = prop + 1.96 * prop_se), width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(vars(year), ncol = 2)
  
# Figure 2
p_main +
  geom_line(aes(x = year)) +
  geom_ribbon(aes(x = year, ymin = prop - 1.96 * prop_se, ymax = prop + 1.96 * prop_se), 
              alpha = 0.2, linetype = 0) + 
  facet_wrap(vars(degree), ncol = 1)


# --- Question 2: GIS, Mapping Data  ---------------------------------------------------------------
# Libraries
x <- c("ggmap", "rgdal", "rgeos", "maptools", "tidyverse", "tmap") 
lapply(x, library, character.only = TRUE)

lnd <- readOGR(dsn = "Creating-maps-in-R-master/data", layer = "london_sport")

# lnd is a SpatialPolygonsDataFrame type. It's contents are accessed with @ rather than $.
plot(lnd) # Automatically takes the polygons part to draw the map.

# We should use base R language because polygon structures do not work well with Tidy yet.
lnd@data[lnd$Partic_Per < 15, ]
lnd@data %>% filter(Partic_Per < 15) # Actually, here it seems to work...

# Let us plot all districts but colour in those with a certain participation rate.
plot(lnd, col = "lightgrey")
plot(lnd[lnd$Partic_Per < 15, ], col = "red", add  = T)
plot(lnd[lnd$Partic_Per > 25, ], col = "green", add  = T)

# Step 2: Plot all the districts that are within 10km of the city centre.
plot(lnd, col = "lightgrey") # Raw graph with all of the map.
centre_london <- gCentroid(lnd[lnd$name == "City of London",]) # Coordinates for the centre of london.

# . Use GBuffer function to, starting with a given location, extend its coordinates according to a certain width.
lnd_buffer <- gBuffer(spgeom = centre_london, width = 10000)

# . Subset all locations within the buffer, i.e. whose centroid is within the lnd_buffer object.
lnd_include <- lnd[lnd_buffer,]
plot(centre_london, lwd = 2, col = "red", add = T)
plot(lnd_buffer, border = "blue", add = T)

# method 1 of subsetting selects any intersecting zones
lnd_central <- lnd[lnd_buffer,] # the selection is too big!
# test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer
# method2 of subsetting selects only points within the buffer
lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)


# Challenge 2


# Find the centre of the london area 
east_coordinate_of_centre_map <- coordinates(gCentroid(lnd))[[1]] 
north_coordinate_of_centre_map <- coordinates(gCentroid(lnd))[[2]] 

# For each district, test if the coordinate provided in London is east and/or north of the centre of the map. 
east <- sapply(coordinates(lnd)[,1], function(x) x > east_coordinate_of_centre_map) 
north <- sapply(coordinates(lnd)[,2], function(x) x > north_coordinate_of_centre_map)

lnd$quadrant <- "unknown" # prevent NAs in result 
lnd$quadrant[east & north] <- "North-East"
lnd$quadrant[!east & north] <- "North-West"
lnd$quadrant[east & !north] <- "South-East"
lnd$quadrant[!east & !north] <- "South-West"

plot(lnd)
plot(lnd[lnd$quadrant == "North-East",], col = "yellow", add = T)
plot(lnd[lnd$quadrant == "South-East",], col = "red", add = T)
plot(lnd[lnd$quadrant == "North-West",], col = "blue", add = T)
plot(lnd[lnd$quadrant == "South-West",], col = "green", add = T)


