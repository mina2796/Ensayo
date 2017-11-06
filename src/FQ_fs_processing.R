# 17 January 2017
# Eva C. Serrano 


# clean up: 
rm(list = ls())
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/src/") 
unlink(dir("./data/"))
library(dplyr)
library(plyr)
source ("FeatureSelection.R")
#Imputation of missing values 

# read all files to list
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/") 
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

# Feature selection HighCorrelation
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/fqfeatured/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, FeatureSelection, type="HighCorr", numvarstoreport=6) # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_hcn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

# Feature selection filter 
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/fqfeatured/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, FeatureSelection, type="RankImp", indepVariable="Oxygene.dissous") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_fin%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

# Feature selection Wrapper
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/fqfeatured/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, FeatureSelection, type="Wrapper", indepVariable="Oxygene.dissous") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_wrn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}