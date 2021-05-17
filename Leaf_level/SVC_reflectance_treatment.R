
library(dplyr)
library(tidyr)
library(pavo)
library(stringr)

# Set paths for data import and R output files ----------------------------
FolderPath <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_14/"

date = "20210514"

workingDirectoryPath <- paste(FolderPath, "R_output", sep="")
metaFilePath <- paste(FolderPath, date, "_DataEntry.csv", sep="")
SVCfolderPath <- paste(FolderPath, "SVC", sep="")
setwd(workingDirectoryPath) # Set working directory so that any output files will be saved here 

# Import meta data --------------------------------------------------------
#reading in the meta data. This might need to be included  - fileEncoding="UTF-8-BOM"
meta <- read.csv(metaFilePath, header = TRUE)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Remove rows where there will be no leaf level data collected
meta <- completeFun(meta, "SVCprefix") #This column might need to be changed. 

#create a list of all the files in the reflectance folder. 
file_list <- list.files(SVCfolderPath) # list all the files in the SVC folder

#This is a for loop that pulls out the necessary spectral data from each .sig file and then binds the rows together. For these .sig files, the first 30 rows contain metadata and should be skipped. Uses Pavo package
dataset <- data.frame() #create empty "dataset" dataframe 
nfile_list = length(file_list) # get the number of files in the SVC folder

for (i in 1:nfile_list){
  temp_data <- read.csv2(paste0(SVCfolderPath, '/', file_list[i]), sep = "", header=F, skip = 30)
  temp_data[1:4] <- lapply(temp_data[1:4], as.numeric)
  temp_data <- as.rspec(temp_data,whichwl = "V1")
  temp_data$id <- prefixObs[i]
  dataset <- rbind(dataset, temp_data)
}

names(dataset) <- c("wavelength","reference","radiance","reflectance","SVC_RAW") #add the names for the column. SVC_RAW indicates the file from which the data was extracted. 

dataset$scan <- str_sub(dataset$SVC_RAW, - 4, - 1)  #Uses stringr package. Grabs the last four digits which indicates scan number
dataset$SVCprefix <- str_sub(dataset$SVC_RAW, 1, - 6)  #Uses stringr package. Grabs the SVC prefic that defines the date of measurement. 

# Wrangle meta data -------------------------------------------------------
#The meta data file is the data entry file for all measurements and includes description informaton on the samples e.g. with pathogen/geneotype etc. Additional measurements could include those from Li-600(stomatal conductance and Fluorescence). Given that we only need the data for the SVC, we will extract this data only and then reshape the data so that it can be combined with the actual spectra data. 
svcColNames = colnames(meta) #get all names of columns in the meta data file
svcColsElement <- which(startsWith(colnames(meta), "SVC_")) #define where the SVC measurement numbers are in the spreadsheet. 

for (i in 1:length(svcColsElement)){
  
  temp_data <-  svcColNames[svcColsElement[i]] #Get the SVC scan column
  meta[,temp_data] <- str_pad(meta[,temp_data], 4, pad = "0") #add 0s so that the scan number matches that from the SVC file
  
}

data_long <- gather(meta[c(1:max(svcColsElement))], leaf, scan, SVC_1:SVC_3, factor_key=TRUE) #uses Tidyr package

data_long <- completeFun(data_long, "scan") #not every plant will have the same number of leaves measured. For example, you could plan to measure three leaves per plant but then you have a really small plant with only one leaf for measuring. This removes any rows with no missing information for additional leaves.  

df_20 <- dplyr::left_join(data_long, data.frame(dataset), by = c("SVCprefix", "scan")) #to ensure this has worked correctly, divide the number of rows of this object by 2177(the number of wavelengths for the SVC). e.g. nrow(df_20)/2177. This number should equal number of individual scans you should have (nrows of data_long)


df_20_long <- df_20 %>%
  group_by(Pathogen, Entry, Actual_Plot, SVCprefix, wavelength) %>%
  summarise(rfl_mean  = mean(reflectance, na.rm = TRUE), 
            rfl_sd = sd(reflectance, na.rm = TRUE)
            )


plt_spectall <- ggplot(df_20, aes(x=wavelength,y=reflectance,color=interaction(ID,leaf))) +   #Or color = ID
  geom_line(show.legend = F,size=.5)+
  scale_y_continuous(bquote("Reflectance (%)"),limits = c(0,70), breaks = seq(0,70,20))+
  scale_x_continuous("Wavelength (nm)",limits = c(300,2500), breaks = seq(300,2500,400))+
  theme.gg.leg+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  facet_wrap(~Genotype)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#FF9999")

plt_specid <- ggplot(df_20_long, aes(x=wavelength,y=rfl_mean,color=as.factor(ID))) +
  geom_line(show.legend = F) +
  scale_y_continuous(bquote("Reflectance (%)"),limits = c(0,70), breaks = seq(0,70,20))+
  scale_x_continuous("Wavelength (nm)",limits = c(300,2500), breaks = seq(300,2500,400))+
  scale_color_manual(values=rep(cbbPalette, 30))+
  theme.gg.leg+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  facet_wrap(~Genotype)

df3 <- subset(df_20, select = -c(scan,Block,Order.within.block,reference, radiance))
df_20_wide <- reshape(df3, idvar = c("leaf","ID","Genotype","Treatment"), timevar = "wavelength", direction = "wide")

df_20_wide$NDVI <- (df_20_wide$reflectance.800 - df_20_wide$reflectance.680) / (df_20_wide$reflectance.800 + df_20_wide$reflectance.680)
df_20_wide$PRI <- (df_20_wide$reflectance.531 - df_20_wide$reflectance.570) / (df_20_wide$reflectance.531 + df_20_wide$reflectance.570)
df_20_wide$date <- as.Date("2020-12-07")

#Boxplot for plot.id
plt_PRI <- ggplot(df_20_wide,aes(Genotype,PRI))+      
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0, aes(colour = interaction(ID,leaf)),size = 1.5, show.legend = FALSE)+
  scale_y_continuous(bquote("PRI"),limits = c(-.2,.1), breaks = seq(-.2,.1,.05))+
  theme.gg.leg+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~Treatment)

plt_NDVI <- ggplot(df_20_wide,aes(Genotype,NDVI))+      
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0, aes(colour = interaction(ID,leaf)),size = 1.5, show.legend = FALSE)+
  scale_y_continuous(bquote("NDVI"),limits = c(.6,1), breaks = seq(.6,1,.1))+
  theme.gg.leg+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~Treatment)


#export csv
write.csv(df_20_long,'20_long.csv')
write.csv(df_20_wide,'20_wide.csv')



