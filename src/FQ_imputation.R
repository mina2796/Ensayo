# Real Dataset
# Test with Fresqueau dataset
# Eva Serrrano
# 05 January 2017

library(stringr)
library (dplyr)
library (plyr)
library(missForest)
library(ForImp)
library(VIM)
library(Amelia)
library (mice)
set.seed(24568)

# clean up: 
rm(list = ls())
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
unlink(dir("./data/"))

# Getting cleaning dataframes
write.table(format(moyenne_macropolluant, trim=T, digits=4), "fqmacro.txt", row.names=F, quote=F, sep=",")
write.table(format(moyenne_micropolluant, trim=T, digits=4), "fqmicro.txt", row.names=F, quote=F, sep=",")
fqmacro2 <- ld(fqmacro) # Requires library(ForImp)
fqmicro2 <- ld(fqmicro) # Requires library(ForImp)
write.table(format(fqmacro2, trim=T, digits=4), "fqmacro.txt", row.names=F, quote=F, sep=",")
write.table(format(fqmicro2, trim=T, digits=4), "fqmicro2.txt", row.names=F, quote=F, sep=",")

# DF with 16 individuals and 14 variables 
fqmaA <- fqmacro[sample(nrow(fqmacro), 16),sample(ncol(fqmacro), 14) ]
write.table(format(fqmaA, trim=T, digits=4), "fqmaA.txt", row.names=F, quote=F, sep=",")
# DF with 2 size 
d <- fqmacro[rep(seq_len(nrow(fqmacro)), 5), ]
write.table(format(d, trim=T, digits=4), "fqmaC.txt", row.names=F, quote=F, sep=",")

# Adding missing values and imputing missing values
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
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")

# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.05) # inject 5% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA05n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.10) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA10n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.15) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA15n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}
#-------------

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.20) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA20n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}
#-------------

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.25) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA25n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}
#-------------

for (j in 1:3){
  modified_list <- llply(list_dataframes, addMissing, 0.30) # inject 10% missing values
  for (i in 1:n)
    write.table(file = sprintf( "incomplete/%s_NA30n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}
#------------------------------------------------
# Imputation of Missing values (solo imputé las primeras tres repeticiones)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau/incomplete")
# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

# Requires library(Amelia)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
modified_list <- llply(list_dataframes, amelia, m = 5) # imputes by irmi
for (i in 1:n){
  write.table(file = sprintf( "imputed/amelia/%s_am.txt", dataframes[i]), modified_list[[i]]$imputations[[1]], row.names = F, sep=",")
}

# Requires library(VIM)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
modified_list <- llply(list_dataframes, hotdeck) # imputes by hotdeck
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "imputed/hotdeck/%s_hd.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

# Requires library(IRMI)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
modified_list <- llply(list_dataframes, irmi) # imputes by irmi
modified_list2 <- lapply(modified_list, function(df) df[sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "imputed/irmi/%s_ir.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

# Requires library(KNN)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
modified_list <- llply(list_dataframes, kNN, numFun = weighted.mean) # imputes by kNN
modified_list2 <- lapply(modified_list, function(df) df[ sapply(df, is.numeric)])
for (i in 1:n){
  write.table(file = sprintf( "imputed/knn/%s_kn.txt", dataframes[i]), modified_list2[[i]], row.names = F, sep=",")
}

# Requires library(mice)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/fresqueau")
modified_list <- llply(list_dataframes, mice, method = "pmm") # imputes by mice
for (i in 1:n){
  write.table(file = sprintf( "imputed/mice/%s_mi.txt", dataframes[i]), complete(modified_list[[i]]), row.names = F, sep=",")
}

