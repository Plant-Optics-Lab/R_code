library(dplyr)
library(tidyr)
library(pavo)
library(stringr)

# Set paths for data import and R output files ----------------------------
# This script is for wrangling and merging data observations with the SVC data. The following layout for the folder is required for the script to work. 
#1. Create a new folder for your project. e.g. Jessie_Field_Strawberry. If you are taking measurements across multiple time points, you may want subfolders per measurement day. E.g. Jessie_Field_Strawberry -> 20210514. This will be the general "FolderPath" that R will 
# use for extracting/exporting the data. 
#2. Within the "FolderPath" folder:
#a) Save the meta data file as a .csv file with the file name layout. "date"_DataEntry.csv. E.g. 20210514_DataEntry.csv.
#b) Create a subfolder "SVC" and place all scans from the SVC in is
#c) Create a subfolder "R_output". This is will be the working directory and to where all data will be exported. 
#Please edit the following "FolderPath", "date" and "descripCol" so to match your project/computer.  

FolderPath <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_14/"
date = "20210514"
descripCol = c(1:13) #in your meta data file (the file containing data entry from making measurements), there will be a series of columns that help you describe the nested structure within your dataset. 
#If treatments are the same, this could just a single column (just sample ID) or it could be a series of columns, e.g. treatment (pathogen/stress), genotype, phenotype or/and individual. 
# Please define which columns are simply describing your plants by creating a index for the description columns. 

# The following lines of code do not need to be adjusted. 
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

# Import SVC data ---------------------------------------------------------
# Create a list of all the files in the SVC folder. 
file_list <- list.files(SVCfolderPath) # list all the files in the SVC folder

# This is a for loop that pulls out the necessary spectral data from each .sig file and then binds the rows together. For these .sig files, the first 30 rows contain metadata and should be skipped. Uses Pavo package
spectralData <- data.frame() #create empty "spectralData" dataframe 
nfile_list = length(file_list) # get the number of files in the SVC folder
prefixObs <- gsub('.{4}$', '', file_list) #Cleans up the file name so that the prefix of the SVC and observation number are extracted only. 

for (i in 1:nfile_list){
  temp_data <- read.csv2(paste0(SVCfolderPath, '/', file_list[i]), sep = "", header=F, skip = 30)
  temp_data[1:4] <- lapply(temp_data[1:4], as.numeric)
  temp_data <- as.rspec(temp_data,whichwl = "V1")
  temp_data$id <- prefixObs[i]
  spectralData <- rbind(spectralData, temp_data)
}

names(spectralData) <- c("wavelength","reference","radiance","reflectance","SVC_RAW") #add the names for the column. SVC_RAW indicates the file from which the data was extracted. 

spectralData$scan <- str_sub(spectralData$SVC_RAW, - 4, - 1)  #Uses stringr package. Grabs the last four digits which indicates scan number
spectralData$SVCprefix <- str_sub(spectralData$SVC_RAW, 1, - 6)  #Uses stringr package. Grabs the SVC prefic that defines the date of measurement. 

# Wrangle meta data by adjusting scan number and reshape file-------------------------------------------------------
#The meta data file is the data entry file for all measurements and includes description information on the samples e.g. with pathogen/genotype etc. Additional measurements could include those from Li-600(stomatal conductance and Fluorescence). Given that we only need the data for the SVC, we will extract this data only and then reshape the data so that it can be combined with the actual spectra data. 
svcColNames = colnames(meta) #get all names of columns in the meta data file
svcColsElement <- which(startsWith(colnames(meta), "SVC_")) #define where the SVC measurement numbers are in the spreadsheet. 

for (i in 1:length(svcColsElement)){
  
  temp_data <-  svcColNames[svcColsElement[i]] #Get the SVC scan column
  meta[,temp_data] <- str_pad(meta[,temp_data], 4, pad = "0") #add 0s so that the scan number matches that from the SVC file
  
}

#reshape meta data to long form. uses Tidyr package. We first gather all the data (descripcol and the SVC columns). We define leaf from which SVC column the data was taken (e.g. SVC 1, 2 or 3). To check that this worked okay, nrow(data_long) = nrow(meta)*length(svcColsElement).
metaLong <- gather(meta[c(descripCol, svcColsElement)], leaf, scan, svcColNames[svcColsElement], factor_key=TRUE) 

metaLong <- completeFun(metaLong, "scan") #not every plant will have the same number of leaves measured. For example, you could plan to measure three leaves per plant but then you have a really small plant with only one leaf for measuring. This removes any rows with no missing information for additional leaves.  

# Merge datasets, summarise spectral data prepare final file for export ------------------------------

mergeMetaSpectra <- left_join(metaLong, data.frame(spectralData), by = c("SVCprefix", "scan")) #Uses dplyr. To ensure this has worked correctly, divide the number of rows of this object by 2177(the number of wavelengths for the SVC). e.g. nrow(df_20)/2177. This number should equal number of individual scans you should have (nrow(data_long))

#Summarise spectral data using dplyr. Currently there are three leaves scanned per plant(provided the plant was big enough). 
metaSpectraLong <- mergeMetaSpectra %>%
  group_by(Pathogen, Entry, Actual_Plot, SVCprefix, wavelength) %>% #define the groups. We want to to average each wavelength, per plant
  summarise(rfl_mean  = mean(reflectance, na.rm = TRUE), #get mean
            rfl_sd = sd(reflectance, na.rm = TRUE) #get standard deviation
            )

#Reshaping the spectral data to wide form. Currently, the data is in long form for the summarising process. When looking/plotting the data, it is easier for the data to be in wide format(one column per wavelength). Here we "spread" the wavelength and rfl_mean column using Tidyr.   
metaSpectraWide <- spread(metaSpectraLong[c(1:6)], wavelength, rfl_mean)

#Prepare dataframe with calculated indices. NDVI and PRI
indicesSVC <- as.data.frame(metaSpectraWide[c(1:4)])
indicesSVC$NDVI <- (metaSpectraWide$`800`-metaSpectraWide$`680`)/(metaSpectraWide$`800`+metaSpectraWide$`680`)
indicesSVC$PRI <- (metaSpectraWide$`531`-metaSpectraWide$`570`)/(metaSpectraWide$`531`+metaSpectraWide$`531`)


# Export data -------------------------------------------------------------
#export csv
metaSpectraExportFileName <- paste("SVC_", date, ".csv", sep="")
indicesExportFileName <- paste("Indices_", date, ".csv", sep="")
write.csv(metaSpectraWide, metaSpectraExportFileName, row.names = F)
write.csv(indicesSVC, indicesExportFileName, row.names = F)




