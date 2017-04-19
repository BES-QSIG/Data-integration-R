###

#Visualising and exploring data:
# - Mapping data
# - Lattice plots


###Note - import step probably in part 1 but copied here

cs_data <- read.csv("LANDSCAPE_AREA_FEATURE_HAB_1978.csv")

cs_locs <- read.csv("CS_locations_false.csv")

ag_data <- read.csv("EnglandWales_1979_2k.csv")


#Look at summaries of the three csv files you have

summary(cs_data)
summary(cs_locs)
summary(ag_data)

#Remove columns that aren't useful from CS data

cs_data <- subset(cs_data, select = -c(YEAR,ID,LAND_CLASS90,EZ_DESC_07))

#Show boxplot of habitat areas

boxplot(cs_data$AREA ~ cs_data$BROAD_HABITAT)

#this is very messy - calculate a more useful statistic - priority area

#flag up priority habitats
cs_data$priority <- 0
cs_data$priority[cs_data$BROAD_HABITAT %in% 24:45] <- 1

cs_data$priority_area <- cs_data$AREA*cs_data$priority

#calculate area of priority habitat per square
cs_areas <- aggregate(priority_area ~ SQUARE + COUNTRY + COUNTY, data = cs_data,FUN = sum)

#add (false) locations where SQUARE column matches between data sets

cs_areas$EASTING <- cs_locs$Easting[match( cs_areas$SQUARE,cs_locs$SQUARE)]
cs_areas$NORTHING <- cs_locs$Northing[match( cs_areas$SQUARE,cs_locs$SQUARE)]


#plot average priority area per country

boxplot(cs_areas$priority_area ~ cs_areas$COUNTRY)

#plot by county - what is obvious?

#histogram

hist(cs_areas$priority_area)

#what does the data distribution look like?


#map the data

library(blighty)
blighty()

points(cs_areas$EASTING/1000, cs_areas$NORTHING/1000, cex = 1, pch = 20, col = "red")
#note the nonsense locations so some might be in the sea!

#change point size by area of habitat
blighty()
points(cs_areas$EASTING/1000, cs_areas$NORTHING/1000, cex = ceiling(cs_areas$priority_area/500000)+1, pch = 20, col = ceiling(cs_areas$priority_area/500000)+1)
#not much spatial pattern due to false locations!


#explore the ag census data

#use lattice to look at relationships between variables

library(lattice)

pairs(ag_data[1:1000,3:7])
#this is only done on the first 1000 rows to speed up the plotting

#which variables are related to each other? does this make sense?

#plot the data
blighty()
points(ag_data$x/1000, ag_data$y/1000)

#yikes! full of points!

#better to convert to raster - this also makes it clear what resolution the data is at

library(raster)

#look at woodland area
ag_raster_df <- ag_data[,c(1,2,7)]

coordinates(ag_raster_df) <- ~x+y
gridded(ag_raster_df) <- TRUE
ag_raster <- raster(ag_raster_df)
plot(ag_raster)
