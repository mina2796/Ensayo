# 18 January 2017
# Eva C. Serrano Balderas
# Test de oredering preprocessing 

# clean up: 
rm(list = ls())
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/src/") 
unlink(dir("./data/"))
library(dplyr)
library(plyr)
library(mice)
library(VIM)
source ("Normalization.R")
#Imputation of missing values 

# read all files to list
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/anormincomp/") 
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)


#............................................
# Primer test NORMALIZACION LUEGO IMPUTACION

# Normalization Min-Max
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_normalise/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="mmnorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_mmn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

# Normalization by Z-score
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_normalise/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="znorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_zsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

#Normalization by Decimal scale
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_normalise/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="decscale") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_dsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

#------------------------------------------------------------
#------------------------------------------------------------
# Imputaciones en datos ALNA normalizados

setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_normalise/") 

# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_normalise_impute/") 

modified_list <- llply(list_dataframes, mice, method = "pmm") # imputes by mice
for (i in 1:n){
  write.table(file = sprintf( "%s_mi.txt", dataframes[i]), complete(modified_list[[i]]), row.names = F, sep=",")
}


modified_list <- llply(list_dataframes, kNN, numFun = weighted.mean) # imputes by kNN
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_kn.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

modified_list <- llply(list_dataframes, irmi) # imputes by irmi
modified_list2 <- lapply(modified_list, function(df) df[sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_ir.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}


modified_list <- llply(list_dataframes, hotdeck) # imputes by hotdeck
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_hd.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

#...........................................................................................
#-------------------------------------------------------------------------------------------
#.............................................................................................
# Segundo test IMPUTACION LUEGO NORMALIZACION 

# read all files to list
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/anormincomp/") 
dataframes <- dir( pattern = "n01.txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

# Imputaciones en datos ALNA normalizados

setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute/") 

#  imputacion por knn
modified_list <- llply(list_dataframes, kNN, numFun = weighted.mean) # imputes by kNN
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_kn.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

# imputacion por hotdeck
modified_list <- llply(list_dataframes, hotdeck) # imputes by hotdeck
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_hd.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

#imputacion por irmi
modified_list <- llply(list_dataframes, irmi) # imputes by irmi
modified_list2 <- lapply(modified_list, function(df) df[sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "%s_ir.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}


# Imputacion por MICE
modified_list <- llply(list_dataframes, mice, method = "pmm") # imputes by mice
for (i in 1:n){
  write.table(file = sprintf( "%s_mi.txt", dataframes[i]), complete(modified_list[[i]]), row.names = F, sep=",")
}

#----------------------------------------
# Normalizacion en datos ALNA imputados

setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute/") 

# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute_normal/") 

# Normalization Min-Max
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute_normal/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="mmnorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_mmn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

# Normalization by Z-score
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute_normal/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="znorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_zsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

#Normalization by Decimal scale
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alna_impute_normal/") 
for (j in 1:1){
  modified_list <- llply(list_dataframes, Normalization, type="decscale") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_dsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

