#Extract multiple sites

#read in csv with point values
library(raster)
library(sp)
library(ncdf4)

#insert your csv name here for site coordinates. 
#csv file columns must include: Site Latitude Longitude
Sites <- read.csv("./site_coords.csv")   #Jessie need to modify

#Need to separate longitude and latitude columns and then bind that to the csv to make spatial points dataframe
Spts <- cbind(as.numeric(Sites$Longitude), as.numeric(Sites$Latitude))

#need to define the coordinate reference system. If you just have a list of lat/long this is probably correct
crsdef <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #for my California GRAPEX sites Jessie may need to modify

#We use the coordinate reference system we just defined to match the long/lat points we have (this tells R that it's
#a spatial feature)
Spts <- SpatialPoints(Spts, proj4string=crsdef)

#combine the points with the csv, then you can keep the information together
Sites1 <- SpatialPointsDataFrame(Spts, Sites)


#write function to process all rasters for SIF layer, can change variable name if you want relative SIF etc.
#If you need to change the band or variable name for the .nc do it in the a portion.
r_tropomi <- function(x) {
  a <- raster(x, varname="SIF")
  ab <- extract(a, Sites1, method='simple')
}

#Make your set of filenames for your .nc files
setwd("./TROPOMI")   #Jessie need to modify
filenames <- list.files("./")
SIF_tropomi <- sapply(filenames, r_tropomi)

#combine with Site ID names (or whatever you're using to distinguish sites from eachother)
ID <- (Sites1$Site)
SiteSIF_tropomi <- cbind(ID, SIF_tropomi)
SiteSIF_tropomi = setNames(data.frame(t(SiteSIF_tropomi[,-1])), SiteSIF_tropomi[,1])

#Save as csv
write.csv(SiteSIF_tropomi, "./Site_SIF_tropomi.csv") #Jessie need to modify