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

#this is very messy - lets focus on a few habitats and group priority habitats together in one category. Habitats to focus on - arable, improved grassland and woodland - expect these to be related to agricultural intensity from ag census

#create columns for each habitat
cs_data$arable_area <- 0 #create empty column to start
cs_data$arable_area[cs_data$BROAD_HABITAT == 4] <- cs_data$AREA[cs_data$BROAD_HABITAT == 4]

cs_data$impgrass_area <- 0 #create empty column to start
cs_data$impgrass_area[cs_data$BROAD_HABITAT == 5] <- cs_data$AREA[cs_data$BROAD_HABITAT == 5]

cs_data$woodland_area <- 0 #create empty column to start
cs_data$woodland_area[cs_data$BROAD_HABITAT %in% c(1,2)] <- cs_data$AREA[cs_data$BROAD_HABITAT %in% c(1,2)] # consider both broadleaf (1) and coniferous (2) woodland

#flag up priority habitats
cs_data$priority <- 0
cs_data$priority[cs_data$BROAD_HABITAT %in% 24:45] <- 1

cs_data$priority_area <- cs_data$AREA*cs_data$priority

#calculate area of priority habitat per square
cs_areas <- aggregate(cbind(arable_area,impgrass_area,woodland_area,priority_area) ~ SQUARE + COUNTRY + COUNTY, data = cs_data,FUN = sum)

#add (false) locations where SQUARE column matches between data sets

cs_areas$EASTING <- cs_locs$Easting[match( cs_areas$SQUARE,cs_locs$SQUARE)]
cs_areas$NORTHING <- cs_locs$Northing[match( cs_areas$SQUARE,cs_locs$SQUARE)]


#plot average arable area per country

boxplot(cs_areas$arable_area ~ cs_areas$COUNTRY)

#plot by county - what is obvious?

#histogram

hist(cs_areas$arable_area)

#what does the data distribution look like?

#what about other habitat areas (improved grassland, woodland and priority habitats)?

#map the data

library(blighty)
blighty()

points(cs_areas$EASTING/1000, cs_areas$NORTHING/1000, cex = 1, pch = 20, col = "red")
#note the nonsense locations so some might be in the sea!

#change point colour by area of habitat
blighty()
points(cs_areas$EASTING/1000, cs_areas$NORTHING/1000, pch = 20, col = ceiling(cs_areas$arable_area/100000)+1)
legend(550,1200, c("0-0.09", "0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-0.99", "1"), col = 1:11, pch = 20, cex = 0.6, title = "Arable land (km2)")
             
      
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

#look at total area of holdings
ag_raster_df <- ag_data[,c(1,2,4)]

coordinates(ag_raster_df) <- ~x+y
gridded(ag_raster_df) <- TRUE
ag_raster <- raster(ag_raster_df)
plot(ag_raster)

#get information about raster
ag_raster

#note resolution - 2km by 2km 

#what is resolution of CS data? 1km sample but 10km spatial resolution - not clear how to process. Need background info and understanding

#for following example decide to average ag census data for 10km described by survey square?








