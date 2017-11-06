#24 October 2016
# Eva C. Serrano Balderas
# Modified 16 January 2017 Non-linear Normalizations: solo una variable no es normalizada y disco de G a F

# clean up: 
rm(list = ls())
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/src/") 
unlink(dir("./data/"))
library(dplyr)
library(plyr)
source ("Normalization.R")
#Imputation of missing values 

# read all files to list
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/anormal/") 
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

# Normalization Min-Max
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise/") 
for (j in 1:3){
modified_list <- llply(list_dataframes, Normalization, type="mmnorm") # normalize by Min-Max
for (i in 1:n){
  write.table(file = sprintf( "%s_mmn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

# Normalization by Z-score
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, Normalization, type="znorm") # normalize by Min-Max
for (i in 1:n){
  write.table(file = sprintf( "%s_zsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
}
}

#Normalization by Decimal scale
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise/") 
for (j in 1:3){
modified_list <- llply(list_dataframes, Normalization, type="decscale") # normalize by Min-Max
for (i in 1:n){
  write.table(file = sprintf( "%s_dsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
}
}

#------------------------------------------------------------
# Noramlizaciones para test de regresion corregido en LaTexis
# Normalization by Decimal scale all except one column
#Normalization by Decimal scale
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise2/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, Normalization, type="decscale") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_dsn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

#Normalization Sigmascale
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise2/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, Normalization, type="signorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_ssn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}

#Normalization softmaxnorm
setwd("F:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/alnormalise2/") 
for (j in 1:3){
  modified_list <- llply(list_dataframes, Normalization, type="softmaxnorm") # normalize by Min-Max
  for (i in 1:n){
    write.table(file = sprintf( "%s_smn%02d.txt", dataframes[i],j), modified_list[[i]], row.names = F, sep=",")
  }
}



