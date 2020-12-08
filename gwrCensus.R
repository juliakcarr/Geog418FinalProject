####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
income.tracts.no0.coords <- sp::coordinates(income.tracts.no0)
#Observe the result:
head(income.tracts.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
income.tracts.no0$X <- income.tracts.no0.coords[,1]
income.tracts.no0$Y <- income.tracts.no0.coords[,2]

###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(income.tracts.no0$Income~income.tracts.no0$Pm2.5, 
                        data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(income.tracts.no0$Income~income.tracts.no0$Pm2.5, 
                data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
income.tracts.no0$localr <- results$localR2

View(income.tracts.no0@data)

tmap_mode("plot")

#Create choropleth map of r-square values
map_r2 <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "localr",
              border.col = "grey32",
              border.alpha = 0.6,
              title = "R2 Values of the \nGWR Model",
              #style = "jenks",
              style = "fixed", breaks = c(0,0.25,0.5,0.75,1.0),
              palette = "RdBu", n = 6) +
  tm_legend(legend.outside=TRUE)

map_r2

tmaptools::palette_explorer() #Tool for selecting pallettes

View(income.tracts.no0@data)
nrow(income.tracts.no0)

#Time for more magic. Let's map the coefficients
income.tracts.no0$coeff <- results$income.tracts.no0.Pm2.5
#Create choropleth map of the coefficients
map_coef <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "coeff",
              border.col = "grey32",
              border.alpha = 0.6,
              title = "Coefficients of the \nGWR Model",
              style = "fixed", breaks = c(-5620000, -500000, 0, 500000, 2535000),
              #style = "jenks",
              palette = "RdBu", n = 4) +
    tm_legend(legend.outside=TRUE)

map_coef

