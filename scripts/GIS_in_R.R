####################
# basics of GIS in R
####################

#######################
# almost all taken from:
# https://www.nickeubank.com/gis-in-r/


install.packages(c("sp", "raster", "rgdal", "maptools"))
install.packages("rgeos", type = "source", configure.args = "--with-geos-config=/Library/Frameworks/GEOS.framework/Versions/Current/unix/bin/geos-config")
# libraries
library (sp)
library (rgdal)
library (raster)
library(rgeos)
library(maptools)
library(RColorBrewer)

################
# spatial points

# coordinates are just x and y on a graph
toy.coordinates <- rbind(c(1.5, 2), c(2.5, 2), c(0.5, 0.5), c(1, 0.25), c(1.5, 0), c(2, 0), c(2.5, 0), c(3, 0.25), c(3.5, 0.5))
toy.coordinates

my.first.points <- SpatialPoints(toy.coordinates)  # ..converted into a spatial object

# R knows how to treat spatial objects
summary(my.first.points)
plot(my.first.points)
coordinates(my.first.points)

# to make the coordinates relate to places in the real world 
# they must be mapped to a "Coordinate Reference System" (CRS)
# the combination of a geographic coordinate system and possibly a projection 
# which is stored using a code called a proj4string
is.projected(my.first.points)  
# Returns `NA` if no geographic coordinate system or projection; returns
# FALSE if has geographic coordinate system but no projection.

# define CRS you want to use
crs.geo <- CRS("+init=EPSG:32633")  # UTM 33N
crs.geo  # prints all parameters

proj4string(my.first.points) <- crs.geo  # define projection system of our data
is.projected(my.first.points)
summary(my.first.points)


# coordinates can be merged with attributes into a spatial data frame
attributes <- data.frame(attr1 = c("a", "b", "z", "d", "e", "q", "w", "r", "z"), attr2 = c(101:109))
attributes

# merge
my.first.spdf <- SpatialPointsDataFrame(my.first.points, attributes)
summary(my.first.spdf)

# can be summarized and subsetted exactly the same as a regular data frame
plot(my.first.spdf[which(my.first.spdf$attr2 > 105), ])  # select if attr2 > 5


# if your data is already in a single file, you can convert to a spatial data frame
# using the coordinates function

# mock imported dataset
sf.df = data.frame(cbind(rnorm(40) * 2 + 13, rnorm(40) + 48), runif(40), rep(month.abb, length.out = 40))
names(sf.df) = c("longitude", "latitude", "attr1", "month")

# specify which columns are the coordinates
coordinates(sf.df) <- c("longitude", "latitude")


##################
# spatial polygons

# polygons are very similar to work with 

# create polyon objects from coordinates.  Each object is a single geometric
# polygon defined by a bounding line.
house1.building <- Polygon(rbind(c(1, 1), c(2, 1), c(2, 0), c(1, 0)))

house1.roof <- Polygon(rbind(c(1, 1), c(1.5, 2), c(2, 1)))


house2.building <- Polygon(rbind(c(3, 1), c(4, 1), c(4, 0), c(3, 0)))

house2.roof <- Polygon(rbind(c(3, 1), c(3.5, 2), c(4, 1)))

house2.door <- Polygon(rbind(c(3.25, 0.75), c(3.75, 0.75), c(3.75, 0), c(3.25, 0)), hole = TRUE)


# create lists of polygon objects from polygon objects and unique ID A
# `Polygons` is like a single observation.
h1 <- Polygons(list(house1.building, house1.roof), "house1")
h2 <- Polygons(list(house2.building, house2.roof, house2.door), "house2")

# create spatial polygons object from lists 
# A SpatialPolygons is like a shapefile or layer.
houses <- SpatialPolygons(list(h1, h2))
plot(houses)

# can add attributes to polygons in the same way
attr <- data.frame(attr1 = 1:2, attr2 = 6:5, row.names = c("house2", "house1"))
houses.DF <- SpatialPolygonsDataFrame(houses, attr)
as.data.frame(houses.DF)  # Notice the rows were re-ordered!
spplot(houses.DF)

# can add CRS as before
crs.geo <- CRS("+init=EPSG:4326")  # geographical, datum WGS84
proj4string(houses.DF) <- crs.geo  # define projection system of our data


##################################################
# Importing and Exporting Spatial Data using rgdal

# We can read in and write out spatial data using:
  
#  readOGR() and writeOGR()

# The parameters provided for each function varies depending on the exact spatial file type you are reading. 
# A shapefile consists of various files, and R expects all those files to be in one directory.

# When reading in a shapefile, readOGR() expects at least the following two arguments:
  
# the path to the folder that contains the files (dsn, data source name)
# the file name without extension (layer)

# set wd
setwd("~/R/r_club/gis_in_r-master/gis_in_r-master/RGIS3_Data")

# read in shape files
palo_alto <- readOGR("palo_alto", "palo_alto")
freeways <- readOGR("palo_alto", "palo_alto_freeways")

plot(palo_alto)
plot(freeways, col = "blue", add = T)

spplot(palo_alto, "PrCpInc", 
       main = "Palo Alto Demographics", 
       sub = "Average Per Capita Income", 
       col = "transparent")


####################################
# setting xlim and ylim (map extent)

# Change these parameters
scale.parameter = 0.5  # scaling parameter. less than 1 is zooming in, more than 1 zooming out. 
xshift = -0.1  # Shift to right in map units. 
yshift = 0.2  # Shift to left in map units. 
original.bbox = palo_alto@bbox  # Pass bbox of your Spatial* Object. 

# reset map edges
edges = original.bbox

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, ]) + yshift


# In `spplot`, set xlim to edges[1,] and ylim to edges[2,]
spplot(palo_alto, "PrCpInc", main = "Palo Alto Demographics", sub = "Average Per Capita Income", 
       col = "transparent", xlim = edges[1, ], ylim = edges[2, ])


# getting the right color pallette is important
display.brewer.all()
my.palette <- brewer.pal(n = 7, name = "Greens")
spplot(palo_alto, "PrCpInc", col.regions = my.palette, cuts = 6, col = "transparent")


# spplot allows users to add multiple layers or symbols using the sp.layout argument

# Create a layer-list
freeway.layer <- list("sp.lines", freeways, col = "black", lwd = 2)

# Plot with layer-list passed to `sp.layout`
spplot(palo_alto, "PrCpInc", sp.layout = freeway.layer, col.regions = my.palette, cuts = 6, col = "transparent")

################
# adding legends
palo_alto_proj <- spTransform(palo_alto, CRS("+init=EPSG:32611"))  # Can't make a scale if not projected!
freeways_proj <- spTransform(freeways, CRS("+init=EPSG:32611"))
freeway.layer.proj <- list("sp.lines", freeways_proj, col = "black", lwd = 2)


palo_alto_proj@bbox  # Check dimensions to help guide offset choices

# scale bar
scale = list("SpatialPolygonsRescale", 
             layout.scale.bar(), 
             scale = 25000, 
             fill = c("transparent", "black"), 
             offset = c(41000, 4100000))

# The scale argument sets length of bar in map units
text1 = list("sp.text", c(41000, 4104000), "0")
text2 = list("sp.text", c(66000, 4104000), "25 km")

# north arrow
arrow = list("SpatialPolygonsRescale", 
             layout.north.arrow(), 
             offset = c(90000, 4150000), 
             scale = 5000)

# plot
spplot(palo_alto_proj, "PrCpInc", 
       sp.layout = list(freeway.layer.proj, scale, text1, text2, arrow), 
       col.regions = my.palette, cuts = 6, col = "transparent")


############################
# buffers and intersections

# set wd
setwd("~/R/r_club/gis_in_r-master/gis_in_r-master/RGIS2_Data")

# read in shape files
districts = readOGR(dsn = "shapefiles", layer = "congressional_districts")
grants = readOGR(dsn = "shapefiles", layer = "federal_grants")

# Re-project so that they match
proj4string(districts)
proj4string(grants)
dist.crs <- CRS(proj4string(districts))
grants.newproj <- spTransform(grants, dist.crs)

# check t see if it worked
proj4string(grants)
proj4string(grants.newproj)

# plot side by side to show new projections
par(mfrow = c(1, 2))
plot(grants, axes = TRUE)
plot(grants.newproj, axes = TRUE)

# overlay plot
par(mfrow = c(1, 1))
plot(districts)
par(new = T)
plot(grants.newproj, type = ".", col = "blue", add = T)
par(new = F)

# over() function tells you where shape files overlap 
grants.districts <- over(grants.newproj, districts)  # Get district data
grants.districts

# combine
grants.newproj
grants.newproj <- spCbind(grants.newproj, grants.districts)
grants.newproj


# create buffers for individual points or combined
buffered.grants.byidTRUE <- gBuffer(grants.newproj, width = 7000, byid = TRUE)
buffered.grants.byidFALSE <- gBuffer(grants.newproj, width = 7000, byid = FALSE)

par(mfcol = c(1, 2))
plot(districts, main = "byid TRUE")
plot(buffered.grants.byidTRUE, col = "red", add = T)

plot(districts, main = "byid FALSE")
plot(buffered.grants.byidFALSE, col = "red", add = T)


# we can trim polygons based on where they overlap 
intersection <- gIntersection(buffered.grants.byidFALSE, districts, byid = TRUE)

#plot
par(mfcol = c(1, 1))
plot(districts)
plot(intersection, col = "blue", add = T)

