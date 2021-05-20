# Welcome to a script for the Li-600: importing, wrangling and exporting the data. 
# We use the Li-600 to get plant physiology information. We collect measurements on stomatal conductance and fluorescence. 
# There two types of fluorescence measurements, one collected during the daytime (light-adapted leaf) and one collected during the night (dark-adapted leaf). 
# This script will be used for importing the raw data, merging it with meta data files and exporting it for analyses. 

# Load libraries ----------------------------------------------------------
library(stringr)
library(tidyr)
library(dplyr)


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

# Import Li-600 data ------------------------------------------------------
#file <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_04_23/Li-600/2021-04-24/Auto_gsw+F_LI_COR_Default_2021_05_06T22_40_41_405Z_1.csv"
Li600 <- read.csv("/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_14/Li-600/Li_600_20210514.csv")
 #headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
 #Li600 <- read.csv(file, skip = 3, header = F)
 #colnames(Li600)= headers

# Import meta data --------------------------------------------------------
#reading in the meta data. This might need to be included  - fileEncoding="UTF-8-BOM"
meta <- read.csv(metaFilePath, header = TRUE)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Remove rows where there will be no leaf level data collected
meta <- completeFun(meta, "SVCprefix") #This column might need to be changed. 

# Wrangle meta data -------------------------------------------------------

fluColNames = colnames(meta)
fluColsElement <- which(startsWith(colnames(meta), "FLU_"))
fluColsElement <- c(20, 21, 22)

for (i in 1:length(fluColsElement)){
  
  temp_data <-  fluColNames[fluColsElement[i]] #Get the FLU scan column
  meta[,temp_data] <- str_pad(meta[,temp_data], 4, pad = "0") #add 0s so that the scan number matches that from the SVC file
  
}

data_long <- gather(meta[c(1:max(fluColsElement))], leaf, scan, FLU_1:FLU_3, factor_key=TRUE)
data_long <- completeFun(data_long, "scan") #to ensure this has worked correctly, divide the number of rows of this object by the number of individual scans you should have. It should equal 2177 (the number of wavelengths for the SVC)

# Li-600 data wrangling ---------------------------------------------------

Li600$scan <- str_pad(Li600$Observation, 4, pad = "0")
Li600red <- Li600[c("scan", "Fo", "Fm","Fs", "Fm.", "Fv.Fm", "ETR", "Tref", "Tleaf", "Qamb", "P1_Fmax")]
Li600red$FV_FM <- (Li600$Fm- Li600$Fo)/Li600$Fm
Li600red$FV_FMp1 <- (Li600$P1_Fmax- Li600$Fs)/Li600$P1_Fmax
Li600red$SVCprefix <- unique(meta$SVCprefix)[1]
Li600red$Time <- "Week5"


df_20 <- left_join(data_long, Li600red, by = c("scan", "SVCprefix"))
df_20  <- completeFun(df_20, "FV_FM")

complete_20210514 <- df_20



