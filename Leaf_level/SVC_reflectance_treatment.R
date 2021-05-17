library(stringr)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(pavo)

# Set paths for data import and R output files ----------------------------
FolderPath <- "/Users/jessie/Dropbox/2020/Strawberries/FieldExp/2021_05_07/"

date = "20210507"

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
file_list <- list.files("C:/Users/cyswong/Documents/UCDavis/Forrestel_exp/Reflectance/20201207/") 

#clean up the file names and extract only the observation number(this is the scan number)
file_list2 <- gsub(".*\\.(.*)\\..*", "\\1", file_list) 

#file <- read.csv2("C:/Users/cyswong/Documents/UCDavis/Forrestel_exp/Reflectance/20201026/20201026.0012.sig", sep = "", header=F, skip = 30)

# dataset <- data.frame(file[1])
# names(dataset) <- "wavelength"
# 
# for (i in 1:length(file_list)){
#   temp_data <- read.csv2(file_list[i], sep = "", header=F, skip = 30)
#   temp_data <- temp_data[4]
#   names(temp_data) <- file_list2[i]
#   dataset <- cbind(dataset, temp_data)
# }

#This for-loop looks up the spectra data per file and binds it together

dataset <- data.frame()
for (i in 1:length(file_list)){
  temp_data <- read.csv2(file_list[i], sep = "", header=F, skip = 30) #skip the first 30 lines as it's just meta data from the instrument. 
  temp_data[1:4] <- lapply(temp_data[1:4], as.numeric) #convert each column to numeric class
  temp_data <- as.rspec(temp_data,whichwl = "V1") 
  temp_data$id <- file_list2[i] #add a column that specifies the filename id of the fiel (this should include the observation/scan number which will be used to match the metadata)
  dataset <- rbind(dataset, temp_data) #all data per file is bound to the "dataset" object. 
}

names(dataset) <- c("wavelength","reference","radiance","reflectance","scan") #add the names for the column 

#dataset[,1:4] <- sapply(dataset[,1:4],as.numeric) 

meta$Leaf.1 <- str_pad(meta$Leaf.1, 4, pad = "0")
meta$Leaf.2 <- str_pad(meta$Leaf.2, 4, pad = "0")
meta$Leaf.3 <- str_pad(meta$Leaf.3, 4, pad = "0")

data_long <- gather(meta, leaf, scan, Leaf.1:Leaf.3, factor_key=TRUE)

df_20 <- merge(data.frame(dataset), data_long, by = "scan") 

df_20_long <- ddply(df_20, c('ID','wavelength',"Genotype",'Treatment'), summarise,
             rfl_mean  = mean(reflectance, na.rm = TRUE),
             rfl_sd = sd(reflectance, na.rm = TRUE)
             )

df_20_long$date <- as.Date("2020-12-07")

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



