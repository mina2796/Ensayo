#05 Octubre 2016
# Eva C. Serrano Balderas

library(stringr)
library (dplyr)
library (plyr)
library(missForest)
set.seed(24568)

# clean up: 
rm(list = ls())
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_Comparison")
unlink(dir("./data/"))
# Function to create a list of df with a certain condition (names)
library (dplyr)
library (plyr)

listing.df <- function(listdf, condition=character){
  selected.list <- llply(listdf, select, contains(condition) )
  return (selected.list)
}

# Function to get the NRMSE results of Processed data
library(missForest)

nrmse.procesed.data <- function (originaldf, proceseddf, nonproceseddf, nameofprocesseddf=character){
  NRMSE <- nrmse(proceseddf, nonproceseddf, originaldf)
  comment(nameofprocesseddf) <- nameofprocesseddf
  results <- as.data.frame(list(comment(nameofprocesseddf), NRMSE)) 
  names(results) <- c("Dataset", "NRMSE")
  return(results)
}

# Function to compute and save nrmse results
tab.nrmse.imputa <- function (pat.orig = character, pat.deform = character, pat.file.process = character, patter.proces =character, pat.name.nrmse = character){
  original <- read.csv(pat.orig)
  deformed <- read.csv(pat.deform)
  setwd(pat.file.process)
  processedDF <- dir(pattern = patter.proces)
  list.processed <- llply(processedDF, read.table, header = T, dec = ".", sep =",")
  n <- length(processedDF)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_Comparison")
  for (i in 1:n){
    nameofprocesseddf = substr(processedDF[i],start = 1,stop = str_length(processedDF[i])-4)
    modified_list <- llply(list.processed, nrmse.procesed.data, originaldf = original, nonproceseddf= deformed, 
                           nameofprocesseddf = nameofprocesseddf )# computes NRMSE
    write.table(file = sprintf(pat.name.nrmse, processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  }
  return()
}


#-------------------------------------------------------------------------------
# Computing NRMSE on imputed dataframes
# Dataset 21, missing 05 n01
tab.nrmse.imputa(pat.orig = "./data/N21.txt",pat.deform = "./data/deformed/N21.txt_NA05n01.txt", pat.file.process = "./data/processed_NA", patter.proces = "N21.txt_NA05n01", pat.name.nrmse = "data/nrmseNA/df21/05n01/%s_NMRSE05%02d.txt"  )
tab.nrmse.imputa(pat.orig = "./data/N21.txt",pat.deform = "./data/deformed/N21.txt_NA05n02.txt", pat.file.process = "./data/processed_NA", patter.proces = "N21.txt_NA05n02", pat.name.nrmse = "data/nrmseNA/df21/05n01/%s_NMRSE05%02d.txt"  )

# Tengo que borrar los antiguous nrmse
# Tengo que aplicar mi function para las demas series
# Tengo que hacer otra function para agregar la columna Missing.rate y concatenar resultados por %de Missingness

# Missing percentage 05
# Esta parte es para obtener mis resultados 
original <- read.csv("./data/N21.txt") 
deformed <- read.csv("./data/deformed/N21.txt_NA05n01.txt")
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_Comparison/data/processed_NA") #need to be defined to get orignldf
processedDF <- dir(pattern = "N21.txt_NA05n01")
list.processed <- llply(processedDF, read.table, header = T, dec = ".", sep = ",")
n <- length(processedDF)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_Comparison")


for (i in 1:n){
  nameofprocesseddf = substr(processedDF[i],start = 1,stop = str_length(processedDF[i])-4)
  modified_list <- llply(list.processed, nrmse.procesed.data, originaldf = original, nonproceseddf= deformed, 
                         nameofprocesseddf = nameofprocesseddf )# computes NRMSE
  write.table(file = sprintf( "data/nrmseNA/05n01/%s_NMRSE05%02d.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
}
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_Comparison/data/nrmseNA/05n01")
dfs.nrmse <- dir(pattern = "N21.txt_NA05n01")
list.nrmse <- llply(dfs.nrmse, read.table, header =T, dec = ".", sep = ",")
n05 <- as.data.frame(bind_rows(list.nrmse, .id = "Missing rate"))
n05$`Missing rate` <- 5
write.table(format(n05, trim=T, digits=4), "nrmse05.txt", row.names=F, quote=F, sep=",")











