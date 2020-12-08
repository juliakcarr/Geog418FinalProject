#Libraries
library("spgwr")
library("spatstat")
library("tmap")
library("gstat")
library("sf")
library("raster")
library("rgdal")
library("e1071")
library("spdep")
library("rgdal")
library("maptools")
library("sp")
library("plyr")
library("lubridate")
library("rgeos")
library("maps")
library("gtable")
library("gridExtra")
library("grid")
library("ggplot2")
library("dplyr")


#Set working directory
dir <- "/Users/juliacarr/Docs/School/UVic/Fall 2020/Geog 418/Final Project/data"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
#NAD83 UTM Zone 10N
pm2.5 <- readOGR(".", "Pm25Sample") 
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))

#Reading in dissemination tract and income data
#Read in census income data:
income <- read.csv("./income.csv", header = T, sep = ",")  
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income") 
#Read in dissemination tract shapefile:
census.tracts <- readOGR(".", "BC_DA") 
#Merge income and dissemination data:
income.tracts <- merge(census.tracts, income, by = "DAUID") 
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))

tmaptools::palette_explorer() #Tool for selecting pallettes

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              border.col = "grey32",
              border.alpha = 0.6,
              title = "Median Income in \nMetro Vancouver",
              style = "jenks",
              palette = "YlGnBu", n = 6) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income


###############################
## Descriptive statistics for PM2.5
mean.pm <- mean(income.tracts$Pm2.5, na.rm = TRUE)
sd.pm <- sd(income.tracts$Pm2.5, na.rm = TRUE) 
median.pm <- median(income.tracts$Pm2.5, na.rm = TRUE)
CoV.pm <- sd.pm/mean.pm
min.pm <- min(income.tracts$Pm2.5, na.rm = TRUE)
max.pm <- max(income.tracts$Pm2.5, na.rm = TRUE)

mean <- round(mean.pm, digits = 3)
sd <- round(sd.pm, digits = 3)
median <- round(median.pm, digits = 3)
CoV <- round(CoV.pm, digits = 3)
min <- round(min.pm, digits = 3)
max <- round(max.pm, digits = 3)

data.for.table1 = data.frame(mean, sd, median, CoV, min, max)
table1 <- tableGrob(data.for.table1, rows = c(""))
t1Caption <- textGrob("Table 1: Descriptive Statistics for 2016 Mean Annual \nPM2.5 in Metro Vancouver", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")
table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)
table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)
grid.arrange(table1, newpage = TRUE)

png("Table1_PM25.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #Print table

## Descriptive statistics for income
mean.income <- mean(income.tracts$Income, na.rm = TRUE)
sd.income <- sd(income.tracts$Income, na.rm = TRUE) 
median.income <- median(income.tracts$Income, na.rm = TRUE)
CoV.income <- sd.income/mean.income
min.income <- min(income.tracts$Income, na.rm = TRUE)
max.income <- max(income.tracts$Income, na.rm = TRUE)

mean <- round(mean.income, digits = 3)
sd <- round(sd.income, digits = 3)
median <- round(median.income, digits = 3)
CoV <- round(CoV.income, digits = 3)
min <- round(min.income, digits = 3)
max <- round(max.income, digits = 3)

data.for.table2 = data.frame(mean, sd, median, CoV, min, max)
table2 <- tableGrob(data.for.table2, rows = c(""))
t2Caption <- textGrob("Table 2: Descriptive Statistics for 2016 Median \nIncome in Metro Vancouver", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")
table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)
table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)
grid.arrange(table2, newpage = TRUE)

png("Table2_Income.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE)
dev.off() #Print table


###############################
## Objective 1: Spatial segregation of income (Global/local Moran's I)

#define neighbourhoods, default: queens case
pm.nb <- poly2nb(income.tracts)
#convert neighbourhoods list to line graph
pm.net <- nb2lines(pm.nb, coords=coordinates(income.tracts))
crs(pm.net) <- crs(income.tracts)
#create weights matrix
pm.lw <- nb2listw(pm.nb, zero.policy = TRUE, style = "W")
print.listw(pm.lw, zero.policy = TRUE)
#Global Moran's I test
mi <- moran.test(income.tracts$Income, pm.lw, zero.policy = TRUE)
mi
# #calculate exected range of spatial autocorrelation
moran.range <- function(pm.lw) {
  wmat <- listw2mat(pm.lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(pm.lw)
#extract mI, eI, and variance from Moran's I test
mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var.morans <- mi$estimate[[3]]
z.morans <- (mI - eI)/sqrt(var.morans)


#local Moran's I test
lisa.test <- localmoran(income.tracts$Income, pm.lw, zero.policy = TRUE)
#pulling individual results of LISA test and joining it to the polygons
income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]

tmaptools::palette_explorer() #Tool for selecting pallettes

map_LISA <- tm_shape(income.tracts) + 
  tm_polygons(col = "Z.Ii", 
              title = "Local Moran's I for \nIncome in Metro Vancouver", 
              style = "fixed", 
              breaks = c( -Inf, -1.96, 1.96, Inf ),
              palette = "RdBu", 
              auto.palette.mapping = FALSE ) +
    tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_LISA

mp <- moran.plot(income.tracts$Income, pm.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Income", 
           ylab="Spatially Lagged Income", quiet=NULL)
if (require(ggplot2, quietly=TRUE)) 
  xname <- attr(mp, "xname") 
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) +
    geom_smooth(formula=y ~ x, method="lm", se = FALSE) +
    geom_hline(yintercept=mean(mp$wx), lty=2) +
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() +
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    xlab(xname) + ylab(paste0("Spatially lagged", xname))



###############################
#Spatial Interpolation

# Create an empty grid where n is the total number of cells
# can change number of cells, will change resolution of interpolated surface
grd <- as.data.frame(spsample(pm2.5, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)


##############################
##Spatial Interpolation with Kriging
# Define the trend model
f.0 <- as.formula(PM25 ~ 1) 
#Create variogram
var.smpl <- variogram(f.0, pm2.5, cloud = FALSE, cutoff = 13000)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=0.015, model="Sph", range=10000, nugget=0))
plot(var.smpl, dat.fit)

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.0, pm2.5, grd, dat.fit)
# Convert kriged surface to a raster object for clipping
r.krig <- raster(dat.krg)
r.m.krig <- mask(r.krig, income.tracts)
# Plot the map
tm_shape(r.m.krig) + 
  tm_raster(n=6, palette="OrRd",  
            title="Predicted PM2.5 (in ug/m^3) \nusing Ordinary Kriging") +
  tm_shape(pm2.5) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r.krig   <- raster(dat.krg, layer="var1.var")
r.m.krig <- mask(r.krig, income.tracts)
tm_shape(r.m.krig) + 
  tm_raster(n=7, palette ="OrRd",
            title="Variance of predicted PM2.5 \n(in squared ug/m^3) using Ordinary Kriging") +tm_shape(pm2.5) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r.krig   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m.krig <- mask(r.krig, income.tracts)
tm_shape(r.m.krig) + 
  tm_raster(n=7, palette ="OrRd",
            title="95% CI of predicted PM2.5 \n(in ug/m^3) using Ordinary Kriging") +tm_shape(pm2.5) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)


###############################
## Point pattern analysis
#pm2.5@data$PM25 <- as.numeric(pm2.5@data$PM25)
pm2.5@data$PM25 <- as.factor(pm2.5@data$PM25)
levels(pm2.5$PM25)
kma <- pm2.5
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]
#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma)
zd
#if there are duplicates, remove them
kma <- remove.duplicates(kma)
#create an "extent" object which can be used to create the observation window for spatstat
kma.ext <- as.matrix(extent(income.tracts)) 
#observation window (first row, second row of kma.ext)
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))
plot(window)
#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)
plot(kma.ppp)

N <- nrow(pm2.5)
##Nearest Neighbour Distance
#window <- as.owin(VanCity)
#kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)
nearestNeighbour <- nndist(kma.ppp)
##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"
##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
nnd = sum(nearestNeighbour$Distance)/N
#mean nearest neighbour for random spatial distribution
    studyArea <- gArea(income.tracts)
    pointDensity <- N/studyArea
    r.nnd = 1/(2*sqrt(pointDensity))
    d.nnd = 1.07453/sqrt(pointDensity)
    R = nnd/r.nnd
    se.nnd <- 0.26136/(sqrt(N*pointDensity))
    z.nnd = (nnd - r.nnd)/se.nnd

    
##K-FUNCTION 
#basic k-function
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)
#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)


##study map 
map_AQ <- tm_shape(income.tracts) + 
            tm_polygons() +
            tm_fill(col= "gray") +
            tm_shape(pm2.5) +
            tm_dots(col="PM25", palette = "OrRd", style = "jenks", title="Sampled PM2.5 (ug/m^3)", size=0.4) + 
            tm_layout(title = "Metro Vancouver Income Tracts \nwith PM2.5 Points (ug/m^3)") +
            tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.6, position = c("RIGHT","TOP")) +
            tm_compass(type = "arrow", position = c(0.01, 0.05), size = 1.5) +
            tm_legend(legend.outside=TRUE)
map_AQ

