##Eva Carmina Serrano Balderas
# Dataset FRESQUEAU
# 10 January 2017
#-------------------------------------

library('ProjectTemplate')
load.project()

#-------------------------------------

library(synthpop)
library(MorseGen)
library(gendata)
library (MASS)
library(mvtnorm)
library(VIM)
library(dplyr)
library(missForest)
library(plyr)
library(mice)
set.seed(24568)

# clean up: 
rm(list = ls())
setwd("./data/") 
unlink(dir("./data/"))

#Imputation of missing values 

# read all files to list
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/detecoutdf/") #need to be defined to get orignldf

dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)
#--------------------------------------------
# Hot deck imputation
modified_list <- llply(list_dataframes, hotdeck) # imputes by hotdeck
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/") #need to be defined to get orignldf
for (i in 1:n){
  write.table(file = sprintf( "fresqueau/outimputed/%s_hd.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}
#-------------------------------------------
# Knn imputation
modified_list <- llply(list_dataframes, kNN, numFun = weighted.mean) # imputes by kNN
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "fresqueau/outimputed/%s_kn.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}
#------------------------------------------
# IRMI imputation
modified_list <- llply(list_dataframes, irmi) # imputes by irmi
modified_list2 <- lapply(modified_list, function(df) df[sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "fresqueau/outimputed/%s_ir.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}
#------------------------------------------
# MICE imputation
# Tuve que separar los datasets que no tenian NA's para poder imputar por MICE
modified_list <- llply(list_dataframes, mice, method = "pmm") # imputes by mice
for (i in 1:n){
  write.table(file = sprintf( "fresqueau/outimputed/%s_mi.txt", dataframes[i]), complete(modified_list[[i]]), row.names = F, sep=",")
}

#.......................................................................................


# read all files to list
setwd("G:/Documents/imputatout/") #need to be defined to get orignldf

dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)
