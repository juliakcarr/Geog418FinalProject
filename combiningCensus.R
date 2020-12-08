#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
# Convert your interpolation into a raster and map it:
tmaptools::palette_explorer() #Tool for selecting pallettes

r <- raster(r.m)
sufaceMap <- tm_shape(r.m) + 
  tm_raster(n=5,palette = "YlOrBr",
            title="PM 2.5 \n(ug/m^3)") +
  tm_shape(pm2.5) + tm_dots(size=0.2)
sufaceMap
#If you have too many cells, 
#you can reduce the number by aggregating values
#if the exctract function is taking forever (check lab video)
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)

#Extract average pm2.5 for each polygon
income.tracts$Pm2.5 <- round(extract(r.m.krig, income.tracts, fun = mean)[,1], 5)

View(income.tracts@data)

income.tracts <- income.tracts[!is.na(income.tracts@data$Pm2.5),]
for(i in 1:nrow(income.tracts)){
  if(income.tracts@data[i,30] < 0){
    income.tracts@data[i,30] <- 0
  }
}

