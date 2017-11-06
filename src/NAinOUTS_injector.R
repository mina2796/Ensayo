##Eva Carmina Serrano Balderas
# Data generator (general)
# 9th June 2016
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
library(msm)
set.seed(24568)

# clean up: 
rm(list = ls())
setwd("./data/") 
unlink(dir("./data/"))

# Function to inject NAs in outlyered data
nainjectinouts <- function (vector){
  mn <- mean(vector) 
  for (i in 1: length(vector)){
    vector[(vector < 5000) | (vector > 9000)] <- NA
  }
  return(vector)
}
NAsinOUTS <- function (data){
  temp <- data
  temp <- apply(data, 2, nainjectinouts)
return(temp)
  }