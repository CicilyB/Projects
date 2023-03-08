##### BIST 5215 Project (12/2/22)

## Dataset Link: https://www.kaggle.com/datasets/lazaro97/biological-invasions/code

dat_species <- read.csv("P:/BIST 5215/Biological Invasions Data/dat_species.csv")

objects(dat_species)
unique(dat_species$kingdom)
sort(unique(dat_species$year))


# Subsets of data
library(ggplot2)
library(dplyr)

dat2000 <- dat_species %>% filter(year == 2000)
glimpse(dat2000)

dat2015 <- dat_species %>% filter(year == 2015)
glimpse(dat2015)



## Libraries and packages

library(tmap)
library(tidyverse)

library(sf)
library(mapview)


# Invasive Species Mapped

mapView(dat2000, xcol="decimalLon", ycol="decimalLat", crs=4269, grid=TRUE)
mapView(dat2015, xcol="decimalLon", ycol="decimalLat", crs=4269, grid=TRUE)



## Transform Data to Spatial Object

spat_species <- st_as_sf(dat_species, coords = c("decimalLon", "decimalLat"),
                         crs=4326)
st_crs(spat_species)


spat2015 <- spat_species %>% filter(year == 2015)



### Point Pattern Analysis ###
library(maptools)
library(raster)
library(spatstat)
library(geodata)

# Change from unprojected coordinates to projected (CRS)

st_crs(spat_species)

proj_species <- st_transform(spat_species, crs = "EPSG:3857")
st_crs(proj_species)
st_is_longlat(proj_species)


# Convert sf object to ppp

proj2015 <- proj_species %>% filter(year == 2015)
st_is_longlat(proj2015)

point2015 <- as.ppp(st_geometry(proj2015))
marks(point2015) <- NULL

plot(point2015, main=NULL, cols=rgb(0,0,0,.2), pch=20)


# Density analysis [code from:
# https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html#density-based-analysis-1]

Q <- quadratcount(point2015, nx= 3, ny=3)

plot(point2015, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE) 

Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=2)  # Plot density raster
plot(point2015, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points


# Distance Analysis
#Avg nearest neighbor analysis
ann.p <- mean(nndist(point2015, k=1))

#ANN vs neighbor order plot

ANN <- apply(nndist(point2015, k=1:100),2,FUN=mean)
plot(ANN ~ eval(1:100), type="b", main = "ANN vs Neighbor Order Plot", las=1)

  
# Getting US Polygon [code from: 
# https://mhallwor.github.io/_pages/basics_SpatialPolygons ] 

states <- raster::getData("GADM", country = "United States", level = 1)
states <- states[states$NAME_1 != "Alaska" & states$NAME_1 != "Hawaii",]
plot(states)

shapefile(states, filename = "states.shp" )

library(rgeos)
library(tmap)
library(usmap)
library(ggplot2)



# Occurrence of biological invasions by state in 2015 visualization

state <- state.abb

plot_usmap(regions = c("states"),
           data = df, values = "Occurrence")

df <- data.frame(state = state.abb[match(dat2015$state, state.name)],
                                   occurrence = dat2015$occurrence)
df <- na.omit(df)

plot_usmap(regions = c("states"),
           data = df, values = "occurrence")



### Spatial Autocorrelation ###

#Moran I Test
library(spdep)

library(tmap)
tm_shape(s.spat) + tm_polygons(style="quantile", col = "occurrence") +
  tm_legend(outside = TRUE, text.size = .8) 

s.spat <- as(spat2015, "Spatial")
class(spat2015)
class(s.spat)

moran.test(spat2015$occurrence, listw = )

nb <- poly2nb(spat2015, queen=TRUE)












####### Appendix ##########

library(terra)
library(sf)

US <-readShapePoly("states.shp")
USw <- as(US, "owin")

X <- as(point2015, "ppp")[USw]
plot(X, main=NULL, cols=rgb(0,0,0,.2), pch=20)






