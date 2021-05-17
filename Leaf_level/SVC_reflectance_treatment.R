
library(dplyr)
library(tidyr)
library(pavo)
library(stringr)

# Set paths for data import and R output files ----------------------------
FolderPath <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_14/"

date = "20210514"
descripCol = c(1:13) #in your meta data file (the file containing data entry from making measurements), there will be a series of columns that help you describe the nested structure within your dataset. If all treatment is the same, this could just a single column for sample ID or it could be a series of columns, e.g. treatment (pathogen/stress), genotype and individual. Please define which columns are simply describing your plants by creating a index per column. 

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

#reshape meta data to long form. uses Tidyr package. We first gather all the data (descripcol and the SVC columns). We define leaf from which SVC column the data was taken (e.g. SVC 1, 2 or 3). To check that this worked okay, nrow(data_long) = nrow(meta)*length(svcColsElement).
data_long <- gather(meta[c(descripCol, svcColsElement)], leaf, scan, svcColNames[svcColsElement], factor_key=TRUE) 

data_long <- completeFun(data_long, "scan") #not every plant will have the same number of leaves measured. For example, you could plan to measure three leaves per plant but then you have a really small plant with only one leaf for measuring. This removes any rows with no missing information for additional leaves.  

df_20 <- left_join(data_long, data.frame(dataset), by = c("SVCprefix", "scan")) #Uses dplyr. To ensure this has worked correctly, divide the number of rows of this object by 2177(the number of wavelengths for the SVC). e.g. nrow(df_20)/2177. This number should equal number of individual scans you should have (nrow(data_long))

#Summarise spectral data using dplyr. Currently there are three leaves scanned per plant(provided the plant was big enough). 
df_20_long <- df_20 %>%
  group_by(Pathogen, Entry, Actual_Plot, SVCprefix, wavelength) %>% #define the groups. We want to to average each wavelength, per plant
  summarise(rfl_mean  = mean(reflectance, na.rm = TRUE), #get mean
            rfl_sd = sd(reflectance, na.rm = TRUE) #get standard deviation
            )

#export csv
write.csv(df_20_long,'20_long.csv')
write.csv(df_20_wide,'20_wide.csv')



