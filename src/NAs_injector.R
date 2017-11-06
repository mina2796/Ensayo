?##Eva Carmina Serrano Balderas
# Data generator (general)
# 25th February 2016
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
set.seed(24568)

# clean up: 
rm(list = ls())
setwd("./data/") 
unlink(dir("./data/"))

# Function to inject missing data (NAs)  

addMissing <- function(data, amount){
  temp <- data
  amount2 <- ifelse(amount<1, (prod(dim(data))*amount), amount)
  if (amount2 >= prod(dim(data))) stop("exceeded data size")
  for (i in 1:amount2) temp[sample.int(nrow(temp), 1), sample.int(ncol(temp), 1)] <- NA
  return(temp)
}  

# ----------------------------------------------------------------------------------------

# Injection of missing values

# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.05) # inject 5% missing values
for (i in 1:n)
  write.table(file = sprintf( "incomplete/%s_NA05n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.10) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA10n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.15) # inject 15% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA15n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.20) # inject 20% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA20n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.25) # inject 25% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA25n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addMissing, 0.30) # inject 30% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA30n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}





