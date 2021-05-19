# Welcome to a script for the Li-600: importing, wrangling and exporting the data. 
# We use the Li-600 to get plant physiology information. We collect measurements on stomatal conductance and fluorescence. 
# There two types of fluorescence measurements, one collected during the daytime (light-adapted leaf) and one collected during the night (dark-adapted leaf). 
# This script will be used for importing the raw data, merging it with meta data files and exporting it for analyses. 

# Load libraries ----------------------------------------------------------


# Set file paths ----------------------------------------------------------
FolderPath <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_14/"
date = "20210514"
descripCol = c(1:13) #in your meta data file (the file containing data entry from making measurements), there will be a series of columns that help you describe the nested structure within your dataset. 
#If treatments are the same, this could just a single column (just sample ID) or it could be a series of columns, e.g. treatment (pathogen/stress), genotype, phenotype or/and individual. 
# Please define which columns are simply describing your plants by creating a index for the description columns. 

# The following lines of code do not need to be adjusted. 
workingDirectoryPath <- paste(FolderPath, "R_output", sep="")
metaFilePath <- paste(FolderPath, date, "_DataEntry.csv", sep="")
Li600folderPath <- paste(FolderPath, "Li-600", sep="")
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


# Import Li-600 data ------------------------------------------------------
Li600 <- read.csv("/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_07/Li-600/Auto_gsw+F_LI_COR_Default_2021_05_10T22_06_37_305Z_1.csv")



