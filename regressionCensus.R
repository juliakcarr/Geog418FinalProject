######Linear Regression##########
#Let's say your dataset with both PM2.5 and Income 
#are stored in a dataset called income.tracts.
#Plot income and PM2.5 from the income.tracts dataset you created
#plot(income.tracts$DEPENDENT~income.tracts$INDEPENDENT)
plot(income.tracts$Income~income.tracts$Pm2.5)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
income.tracts.no0 <-  income.tracts[which(income.tracts$Pm2.5 > 0), ]

#Now plot the data again
plot(income.tracts.no0$Income~income.tracts.no0$Pm2.5)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
#Add the regression model to the plot you created
plot(income.tracts.no0$Income~income.tracts.no0$Pm2.5)
abline(lm.model, col = "red")
#Get the summary of the results
summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
income.tracts.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
income.tracts.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(income.tracts.no0)

tmaptools::palette_explorer() #Tool for selecting pallettes

#Now, create choropleth map of residuals
map_resid <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "residuals",
              border.col = "grey32",
              border.alpha = 0.6,
              title = "Residuals of the \nRegression Model",
              style = "jenks",
              palette = "RdBu", n = 6) +
  tm_legend(legend.outside=TRUE)

map_resid

#Global Moran's on residuals
#define neighbourhoods, default: queens case
pm.nb.res <- poly2nb(income.tracts.no0)
#convert neighbourhoods list to line graph
pm.net.res <- nb2lines(pm.nb.res, coords=coordinates(income.tracts.no0))
crs(pm.net.res) <- crs(income.tracts.no0)
#create weights matrix
pm.lw.res <- nb2listw(pm.nb.res, zero.policy = TRUE, style = "W")
print.listw(pm.lw.res, zero.policy = TRUE)
#Global Moran's I test
mi.res <- moran.test(income.tracts.no0$Income, pm.lw.res, zero.policy = TRUE)
mi.res
#calculate exected range of spatial autocorrelation
# moran.range.res <- function(pm.lw.res) {
#   wmat.res <- listw2mat(pm.lw.res)
#   return(range(eigen((wmat.res + t(wmat.res))/2)$values))
# }
# moran.range(pm.lw.res)
#extract mI, eI, and variance from Moran's I test
mI.res <- mi.res$estimate[[1]]
eI.res <- mi.res$estimate[[2]]
var.morans.res <- mi.res$estimate[[3]]
z.morans.res <- (mI.res - eI.res)/sqrt(var.morans.res)


