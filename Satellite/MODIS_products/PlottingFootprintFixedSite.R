# Extracting MODIS data for a fixed site and assessing the footprint of the data extracted. 

# When extracting data from a fixed site using <https://modis.ornl.gov/sites/>, the area surrounding the site is also extracted. The area size is a 8500m^2 = (17 * 17 pixels) * 500m(resolution of MODIS). There is a total 289 columns corresponding to the MODIS data. 
# Typically we extract the centre pixel, (column 145 out of 289 above mentioned columns) as that is the pixel where the fixed site (e.g. flux tower) should sit. 
# Depending on your site, this centre pixel might not represent the heterogeneity in your site. 
# This script will plot all pixels and allow you to assess the variation of the observation surrounding your fixed site. 


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table) #this is a faster approach to importing from csv. 

# Import Data and set working directory -----------------------------------
setwd("/Users/jessie/Documents/2020/Data/MODIS/CZO2/")

rawData_orig <- fread("filtered_scaled_Gpp_500m.csv") #import original rawdata
rawData <- rawData_orig #create copy of rawData file which will be used for working with the data
MODIScol = rawData %>% #Get the names of the columns with MODIS values. We will have to clean up the data later
  select(V6:V294) %>%
  names
# Create time columns and convert raw values columns to numeric ----------------------------------------------------------
GPPrescale <- function(x) {(as.numeric(x)*1000)/8} #this is a function to convert the raw data to GPP values. If you are using a different MODIS product, you will need to adjust this function. 

manipulateData <- rawData %>%
  dplyr::mutate(Year = as.numeric(paste(str_sub(V3, 2, 5))), 
                DayOfYear = as.numeric(str_sub(V3, 6, 8)), 
                originList = paste(Year, "-01-01", sep = ""), #this is used for converting year and column to date
                Date = as.Date(DayOfYear, origin = originList), 
                month = lubridate::month(Date), 
                across(MODIScol, GPPrescale)) #warnings will pop up if there are non-numeric values as they get converted to "NA". This warning is fine because we want to remove any chanracters in the dataset.


# Summarise MODIS data to month -------------------------------------------
summarisedDF <- manipulateData %>% 
                group_by(Year, month) %>% 
                summarise(across(V6:V294, mean, na.rm = T)) #get the mean per column of data, removing NAs is important

# Plot footprint ----------------------------------------------------------
dfx = as.data.frame(matrix(0, 289, 2)) #create zeros dataframe for coordinates of plot
for (i in 1:17){
  for(j in 1:17) {
    count = (i-1) * 17 + j
    dfx[count, 1] =i
    dfx[count, 2] =j
  }
}#create coordinates for tile plot
names(dfx) <- c("ycoor", "xcoor")
summarisedDFsubset <- summarisedDF[summarisedDF$month==9, ] #subset data to look at just one month

for (i in 1:21){
rownum = i
df2 <- df
  
df2[,3] = unlist(summarisedDFsubset[rownum,c(3:291)])

# plot with geom_tiles
plot <- ggplot(data=df2,aes(x=V1,y=V2,fill=V3))+
  geom_tile()+
  geom_point(aes(x=9, y=9), colour="white", shape = 23, fill = "white", size=6)+ #plotting the flux tower
  xlab("")+
  ylab("")+
  scale_fill_gradientn(colours = rainbow(4), limits=c(1, 8))+
  xlim(1, 17)+
  ylim(1, 17)+
  ggtitle(paste0(summarisedDFsubset[rownum, "Year"], "_", summarisedDFsubset[rownum, "month"]))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

pdf(paste0(rownum, ".pdf"))
print(plot)
dev.off()
}

plot_tiles <- function(dfLong, dfWide, rownum){
  plot <- ggplot(data=dfLong,aes(x=V1,y=V2,fill=V3))+
    geom_tile()+
    geom_point(aes(x=9, y=9), colour="black", shape = 23, fill = "white", size=6)+
    xlab("")+
    ylab("")+
    scale_fill_gradientn(colours = rainbow(4), limits=c(1, 8), name = "GPP")+
    xlim(1, 17)+
    ylim(1, 17)+
    ggtitle(paste0(dfWide[rownum, "Year"], "_",dfWide[rownum, "month"]))+
     theme_bw() +
    #remove plot background
    theme(plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
    axis.ticks.y = element_blank(), axis.text.y = element_blank())
  plot
  # pdf(paste0(rownum, ".pdf"))
  # print(plot)
  # dev.off()
}


plot_tiles(df2, summarisedDFsubset, 8)
summarisedDFsubset <- summarisedDF[summarisedDF$month==7, ] #subset data to look at just one month


for(i in 1:12){
  monthNum = i
  
summarisedDFsubset <- summarisedDF[summarisedDF$month==monthNum, ] #subset data to look at just one month

numRows = nrow(summarisedDFsubset)
emptydf=as.data.frame(matrix(0, 289, numRows))
for (i in 1:numRows){
  rownum = i
  emptydf[,i] = unlist(summarisedDFsubset[rownum,c(3:291)]) 
}
summarisedDFsubset$timeID <- paste0(summarisedDFsubset$Year, "_", summarisedDFsubset$month)
names(emptydf) <- summarisedDFsubset$timeID

x = cbind(dfx, emptydf)

dfx$combined <- paste0(dfx$ycoor, "_", dfx$xcoor)
data_long <- gather(x, time, combined, names(emptydf)[1]: tail(names(emptydf), n=1), factor_key=TRUE)

plot = ggplot(data=data_long,aes(x=xcoor,y=ycoor,fill=combined))+
  geom_tile()+
  geom_point(aes(x=9, y=9), colour="white", shape = 23, fill = "white", size=4)+ #plotting the flux tower
  xlab("")+
  ylab("")+
  scale_fill_gradientn(colours = rainbow(4), limits=c(1, 8), name = "GPP")+
  xlim(1, 17)+
  ylim(1, 17)+
  #ggtitle(paste0(summarisedDFsubset[rownum, "Year"], "_", summarisedDFsubset[rownum, "month"]))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  facet_wrap(~time)

pdf(paste0(monthNum, ".pdf"))
print(plot)
dev.off()
}
