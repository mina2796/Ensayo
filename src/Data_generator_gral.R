##Eva Carmina Serrano Balderas
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

set.seed(24568)
# Functions to generate datasets.
# Size of the dataframe (ncol, nrow) and the correlation between variables are defined by user

# Function to inject missing data (NAs)  

addMissing <- function(data, amount){
  temp <- data
  amount2 <- ifelse(amount<1, (prod(dim(data))*amount), amount)
  if (amount2 >= prod(dim(data))) stop("exceeded data size")
  for (i in 1:amount2) temp[sample.int(nrow(temp), 1), sample.int(ncol(temp), 1)] <- NA
  return(temp)
}  

# ----------------------------------------------------------------------------------------
# Function which returns a matrix, and takes column vectors as arguments for mean and sd
normv <- function( n , mean , sd ){
  out <- rnorm( n*length(mean) , mean = mean , sd = sd )
  return( matrix( out, ncol = n , byrow = FALSE ) )
}

# ----------------------------------------------------------------------------------------
## My function to generate dataframe with normal distribution. 
gendf <- function(ncol, nrow, var.names=NULL){
  if (ncol < 2) stop("ncol must be greater than 1") 
  matx <- replicate (ncol, rnorm(nrow))   # create matrix of normal random numbers
  DF <- data.frame(matx)                  # transform the matrix in dataframe
  if (!is.null(var.names)) colnames(DF) <- var.names
  return(DF)
}

# ----------------------------------------------------------------------------------------
# My 2nd function to generate dataframe using sample
gendf2 <- function(ncol, nrow, valin, valfin, var.names=NULL){
  if (ncol < 2) stop("ncol must be greater than 1")
  matx <- replicate (ncol, sample((valin : valfin), size = nrow, replace =TRUE))    #  creates a matrix of normal random numbers
  DF <- data.frame(matx)      # transform the matrix in dataframe
  if (!is.null(var.names)) colnames(DF) <- var.names
  return(DF)
}
df2 <- gendf2(4, 3, 5, 15)
df2

#----------------------------------------------------------------------------------------
# Function to generate dataframe using 'rmvnorm' function of the Package mvtnorm. This is the function that I finally use

gendf3 <- function (numrow, numcol, mean, valuecorr, var.names = NULL){ # valuecorr is the user-defined covariance value
  require (mvtnorm)
  cov.mat <- diag(0,numcol)                                            # construct the correlation matrix
  cov.mat[lower.tri(cov.mat)] <- c(rep(valuecorr, numcol))
  cov.mat[upper.tri(cov.mat)] <- c(rep(valuecorr, numcol))
  diag(cov.mat) <- c(rep(1, numcol))
  normal.mu <- c(rep(0, numcol))
  matriz <- rmvnorm(numrow, mean = mean, sigma = cov.mat)
  DF <- data.frame(matriz)
  if (!is.null(var.names)) colnames (DF) <- var.names
  return (DF)
}

#------------------------------------------------------------
# Function to generate NON-NORMAL  dataframes from chi-Squred distribution. 

gendf2 <- function(nr, nc,sca, var.names = NULL){
  if (nc < 2) stop("ncol must be greater than 1")
  output <- matrix(ncol = nc, nrow = nr)
  for(i in 1:nr){
    output[i,] <- rweibull(nc,1, sca)
  }
  DF <- data.frame(output)
  if (!is.null(var.names)) colnames(DF) <- var.names
  return(DF)
}
#mediana <- c(sample(0.001:800, ncol, TRUE))
nro <-200
nco <- 8
dfr <- 10
vin <- 0.02
vfi <- 500
sca2 <- sample(vin:vfi,nc,replace = TRUE)
g <- gendf2(nr, nc,sca2)
#-------------------------------------------------  
# Function to generate Dataframe for FEATURE SELECTION
gendf4 <- function (numrow, numcol, mean, valuecorr){ # valuecorr is the user-defined covariance value
  require (mvtnorm)
  cov.mat <- diag(0,numcol)                                            # construct the correlation matrix
  cov.mat[lower.tri(cov.mat)] <- c(rep(valuecorr, numcol))
  cov.mat[upper.tri(cov.mat)] <- c(rep(valuecorr, numcol))
  diag(cov.mat) <- c(rep(1, numcol))
  normal.mu <- c(rep(0, numcol))
  matriz <- rmvnorm(numrow, mean = mean, sigma = cov.mat)
  DF <- data.frame(matriz)
  name <- vector()
  for (i in 1: numcol){
    name[i] <- paste('Y', i, sep='')
  }
  colnames(DF) <- name
  #if (!is.null(var.names)) colnames (DF) <- var.names
  return (DF)
}
#-------------------------------------------------------------------------------------------

name <- vector()
for (i in 1: ncol){
  name[i] <- paste('Y', i, sep='')
}
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Generating and saving synthetic data NORMAL

# Generate dataset with 21 rows and 8 columns
nrow <- 21                                      # set number of rows
ncol <- 8                                       # set number of columns
mediana <- c(sample(0.001:800, ncol, TRUE))     # set random values for the vector mean
n21 <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
n21 <- gendf3(nrow, ncol, mediana, 0.75)         # generates df with correlation of 0.1

write.table(format(n21, trim = T, digits = 4), file = "data/N21.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 600 rows and 30 columns
nrow <- 600                                      # set number of rows
ncol <- 30                                       # set number of columns
mediana <- c(sample(0.0001:600, ncol, TRUE))     # set random values for the vector mean
n600 <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
write.table(format(n600, trim = T, digits = 4), file = "data/N600.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 4000 rows and 53 columns
nrow <- 4000                                     # set number of rows
ncol <- 53                                       # set number of columns
mediana <- c(sample(0.001:900, ncol, TRUE))      # set random values for the vector mean
n4000 <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
write.table(format(n4000, trim = T, digits = 4), file = "data/N4000.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 20 000 rows and 98 columns
nrow <- 20000                                      # set number of rows
ncol <- 98                                       # set number of columns
mediana <- c(sample(0.0001:850, ncol, TRUE))     # set random values for the vector mean
n20000 <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
write.table(format(n20000, trim = T, digits = 4), file = "data/N20000.txt", row.names=F, sep = ",") # Write df on working directory


#----------------------------------------------
# Generating NON NORMALIZED synthetic data 
# Generate dataset with 21 rows and 8 columns
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData")
nrow <- 21                                      # set number of rows
ncol <- 8                                       # set number of columns
sca2 <- sample(0.001:700,ncol,replace = TRUE)
n21 <- gendf2(nrow, ncol, sca2)         # generates df with correlation of 0.1
write.table(format(n21, trim = T, digits = 4), file = "data/anormal/AL21.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 600 rows and 30 columns
nrow <- 600                                      # set number of rows
ncol <- 30                                       # set number of columns
sca2 <- c(sample(0.0001:600, ncol, TRUE))     # set random values for the vector mean
n600 <- gendf2(nrow, ncol, sca2)         # generates df with correlation of 0.1
write.table(format(n600, trim = T, digits = 4), file = "data/anormal/AL600.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 4000 rows and 53 columns
nrow <- 4000                                     # set number of rows
ncol <- 53                                       # set number of columns
sca2 <- c(sample(0.001:900, ncol, TRUE))      # set random values for the vector mean
n4000 <- gendf2(nrow, ncol, sca2)         # generates df with correlation of 0.1
write.table(format(n4000, trim = T, digits = 4), file = "data/anormal/AL4000.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 20 000 rows and 98 columns
nrow <- 20000                                      # set number of rows
ncol <- 98                                       # set number of columns
mediana <- c(sample(0.0001:850, ncol, TRUE))     # set random values for the vector mean
n20000 <- gendf2(nrow, ncol, sca2)         # generates df with correlation of 0.1
write.table(format(n20000, trim = T, digits = 4), file = "data/anormal/AL20000.txt", row.names=F, sep = ",") # Write df on working directory

#*************************************************************************
# Generating Categorical data for CLASSIFICATION tests
# Agrege la columna "class" en los datos ya procesados y en los datos originales no-deformados
class.column <- function (df){
  class <- sample(c("very-good", "good", "average", "bad","very-bad"), nrow(df), replace = TRUE)
  df <- cbind(class, df)
  return(df)
}
#-----------------------------------------------------------------------
# Loding ORIGINAL DATA 
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_comparison/data/original")
processedDF <- dir(pattern = ".txt")
list.processed <- llply(processedDF, read.table, header = T, dec =".", sep=",")
n <- length(list.processed)
for (i in 1:n){
  modified_list <- llply(list.processed, class.column)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/")
  write.table(file = sprintf("discrete/original/%s_D.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  
}
#------------------------------------------------------------------------
# Loding PROCESSED_NA DATA 
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_comparison/data/processed20000_NA")
processedDF <- dir(pattern = ".txt")
list.processed <- llply(processedDF, read.table, header = T, dec =".", sep=",")
n <- length(list.processed)
for (i in 1:n){
  modified_list <- llply(list.processed, class.column)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/")
  write.table(file = sprintf("discrete/processed_NA/%s_D.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  
}
#------------------------------------------------------------------------
# Loding PROCESSED_OUT DATA 
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_comparison/data/processed_OUT")
processedDF <- dir(pattern = ".txt")
list.processed <- llply(processedDF, read.table, header = T, dec =".", sep=",")
n <- length(list.processed)
for (i in 1:n){
  modified_list <- llply(list.processed, class.column)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/")
  write.table(file = sprintf("discrete/processed_OUT/%s_D.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  
}

#------------------------------------------------------------------------
# Loding PROCESSED_AL DATA (Non normalized data) 
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_comparison/data/processed_AL")
processedDF <- dir(pattern = ".txt")
list.processed <- llply(processedDF, read.table, header = T, dec =".", sep=",")
n <- length(list.processed)
for (i in 1:n){
  modified_list <- llply(list.processed, class.column)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/")
  write.table(file = sprintf("discrete/processed_AL/%s_D.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  
} 
#------------------------------------------------------------------------
# Loding PROCESSED_FS DATA (Feature selected data) 
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/Stats_comparison/data/processed_FS")
processedDF <- dir(pattern = ".txt")
list.processed <- llply(processedDF, read.table, header = T, dec =".", sep=",")
n <- length(list.processed)
for (i in 1:n){
  modified_list <- llply(list.processed, class.column)
  setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/")
  write.table(file = sprintf("discrete/processed_FS/%s_D.txt", processedDF[i],i), modified_list[[i]], row.names = F, sep=",")
  
} 
#---------------------------------------------------------------------------
#*************************************************************************
# Generating datasets for FEATURE SELECTION TESTS
# Use of function gendf2 and set the correlation matrix at 1, so data will be highly correlated
# Generate dataset with 21 rows and 8 columns
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/")
nrow <- 21                                      # set number of rows
ncol <- 5                                       # set number of columns
ncol2 <- 3                                      #  Num of cols for correlated data 
mediana <- c(sample(0.001:800, ncol, TRUE))     # set random values for the vector mean
mediana2 <- c(sample(0.001:800, ncol2, TRUE))
n21a <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
n21b <- gendf4(nrow, ncol2, mediana2, 0.75)         # generates df with correlation of 0.1
n21 <- cbind(n21a, n21b)

write.table(format(n21, trim = T, digits = 4), file = "data/featured/N21.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 600 rows and 30 columns
nrow <- 600                                      # set number of rows
ncol <- 21                                       # set number of columns
ncol2 <- 9
mediana <- c(sample(0.0001:600, ncol, TRUE))     # set random values for the vector mean
mediana2 <- c(sample(0.0001:600, ncol2, TRUE))     # set random values for the vector mean
n600a <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
n600b <- gendf4(nrow, ncol2, mediana2, 0.75)
n600 <- cbind(n600a, n600b)
write.table(format(n600, trim = T, digits = 4), file = "data/featured/N600.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 4000 rows and 53 columns
nrow <- 4000                                     # set number of rows
ncol <- 37                                       # set number of columns
ncol2 <- 16
mediana <- c(sample(0.001:900, ncol, TRUE))      # set random values for the vector mean
mediana2 <- c(sample(0.001:900, ncol2, TRUE))
n4000a <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
n4000b <- gendf4(nrow, ncol2, mediana2, 0.75)
n4000 <- cbind(n4000a,n4000b)
write.table(format(n4000, trim = T, digits = 4), file = "data/featured/N4000.txt", row.names=F, sep = ",") # Write df on working directory

# Generate dataset with 20 000 rows and 98 columns
nrow <- 20000                                      # set number of rows
ncol <- 69                                       # set number of columns
ncol2 <- 29
mediana <- c(sample(0.0001:850, ncol, TRUE))     # set random values for the vector mean
mediana2 <- c(sample(0.0001:850, ncol2, TRUE))
n20000a <- gendf3(nrow, ncol, mediana, 0.1)         # generates df with correlation of 0.1
n20000b <- gendf4(nrow, ncol2, mediana2, 0.75)
n20000 <- cbind(n20000a, n20000b)
write.table(format(n20000, trim = T, digits = 4), file = "data/featured/N20000.txt", row.names=F, sep = ",") # Write df on working directory

