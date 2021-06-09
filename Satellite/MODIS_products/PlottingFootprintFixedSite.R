# Extracting MODIS data for a fixed site and assessing the footprint of the data extracted. 

# When extracting data from a fixed site using <https://modis.ornl.gov/sites/>, the area surrounding the site is also extracted. The area size is a 8500m^2 = (17 * 17 pixels) * 500m(resolution of MODIS). There is a total 289 columns corresponding to the MODIS data. 
# Typically we extract the centre pixel, (column 145 out of 289 above mentioned columns) as that is the pixel where the fixed site (e.g. flux tower) should sit. 
# Depending on your site, this centre pixel might not represent the heterogeneity in your site. 
# This script will plot all pixels and allow you to assess the variation of the observation surrounding your fixed site. 


# Load libraries ----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table) #this is a faster approach to importing from csv. 

# Import Data and set working directory -----------------------------------
setwd("/Users/jessie/Documents/2020/Data/MODIS/CZO2/")

ValuesData <- fread("filtered_scaled_Gpp_500m.csv", select = c(6:294))
TimeData <- fread("filtered_scaled_Gpp_500m.csv", select = c(1:5))

# Create time columns ----------------------------------------------------------

tdata <- transform(TimeData, Year = substr(V3, 2, 5), DayOfYear = substr(V3, 6, 8))
tdata$Year <- as.numeric(tdata$Year)
originList <- paste0(tdata$Year, "-01-01")
tdata$DayOfYear <- as.numeric(tdata$DayOfYear)

tdata$date <- as.Date(tdata$DayOfYear, origin = originList)
tdata$month <- lubridate::month(tdata$date) #create month column


# Summarise MODIS data to month -------------------------------------------
ValuesList <- lapply(cbind(tdata$Year, tdata$month, ValuesData), function(x) as.numeric(x)) #convert all columns to numeric



# Plot footprint ----------------------------------------------------------


