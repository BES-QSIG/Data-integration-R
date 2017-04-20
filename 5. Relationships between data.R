## 3. Relationships between data from different datasets

# Use joined data

#Note - create joined data here but would use that from section 4

#Have summarised ag census to 10km

ag_data$x_10km <- floor(ag_data$x/ 10000)*10000
ag_data$y_10km <- floor(ag_data$y/ 10000)*10000

ag_10km <- aggregate(. ~ x_10km + y_10km, data = ag_data, FUN = mean)


#join to CS data

library(sqldf)

cs_with_ag <- sqldf("select * 
                    from cs_areas as t1
                    left join ag_10km as t2
                    on t1.EASTING = t2.x_10km
                    and t1.NORTHING = t2.y_10km")


####
##start section 5 proper

#lots missing as Ag data only for England and Wales

#remove missing values

cs_with_ag2 <- cs_with_ag[complete.cases(cs_with_ag),]
#124 obs left

#use lattice plots again to visualise relationships

#first remove unwanted variables

cs_with_ag3 <- subset(cs_with_ag2, select = -c(x_10km,y_10km,x,y))

pairs(cs_with_ag3[,-c(1,2,3)]) # don't bother plotting country/county data

#lots of relationships to view
#at the mo no look particularly strong due to false cs locations!

#can look at correlations 

cor(cs_with_ag3[,-c(1,2,3)])
#matrix is quite hard to see

#start to think about potential relationships e.g. we would expect woodland area in CS to be correlated with woodland area in ag census...

plot(cs_with_ag3$woodland_area, cs_with_ag3$Woodland.on.holding..c8.)

cor(cs_with_ag3$woodland_area, cs_with_ag3$Woodland.on.holding..c8.)

#negative!

#for most of the relationships between cs and ag census variables correlation might be most appropriate to use - not clear which variable would affect which. The exception is relationships with easting and northing, it is clear that easting can affect arable area but not the other way round! In this case, a regression model can be built


#use simple linear model (regression)

lm1 <- lm(arable_area ~ EASTING, data = cs_with_ag3)

summary(lm1)


#consider other relationships that can be investigated with regression relationships. Extensions to include multiple predictors or generalised linear models could be considered e.g. a zero inflated distribution could be used to describe the area of priority habitat







