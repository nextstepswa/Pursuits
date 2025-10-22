# From: https://www.rpubs.com/EricKrantz/387172

library(rgdal)

# Coordinates are incident based
sp.coords <- WSDOT.raw %>%
  group_by(`Collision Report Number`) %>%
  summarize(Easting = first(`State Plane X`), 
            Northing = first(`State Plane Y`))

# Convert the coordinates to a spatial object:
coordinates(sp.coords) <- ~ Easting + Northing

# Specify the projection of the original data:
#   This should be given with the dataset, or go to:
#   http://spatialreference.org/ref/?search=florida and search
# proj4string(dat) <- CRS("+init=epsg:2236") # NAD_1983_StatePlane_Florida_East_FIPS_0901_Feet

# https://spatialreference.org/ref/epsg/2286 # For WA South

proj4string(sp.coords) <- CRS("+init=epsg:2286") # NAD_1983_StatePlane_Washington_South

# Specify the transformation you want (i.e., the projection of the data you want to convert to)
#   Note that lat/long (WGS84) is epsg:4326
sp.coords <- spTransform(sp.coords, CRS("+init=epsg:4326")) #WGS84

ll.coords <- as.data.frame(sp.coords) %>% 
  mutate(longitude = Easting, latitude = Northing) %>% 
  select(-Easting, -Northing)

# Show the data summary and provide general location of the points by taking the mean of the lat and long
ll.coords %>% summarize(lon = mean(longitude), lat = mean(latitude))

rm("sp.coords")
