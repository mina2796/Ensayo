##Eva Carmina Serrano Balderas
# Data generator (general)
# 27th May 2016
#-------------------------------------

library('ProjectTemplate')
load.project()

#-------------------------------------
# clean up: 
rm(list = ls())

setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/src")
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
source ("outlier.R")
set.seed(24568)

setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/")
setwd("./data/") 
unlink(dir("./data/"))


# Function to inject outliers in a vector
add.outlier.to.vector <- function(vector, amount) {
  amount <- amount/2
  amount2 <- ifelse(amount<1, (round(length(vector)*amount)), (round((length(vector)*amount)/100)))
  if (amount2 >= length(vector)) stop("exceeded data size")
  cells.to.modify <- sample(1:length(vector),amount2, replace = F)
  mean.val <- mean(vector)
  sd.val <- sd(vector)
  min.val <- mean.val - 4 * sd.val
  max.val <- mean.val + 4 * sd.val 
  vector[cells.to.modify] <- rtnorm((amount2), lower = - Inf, upper = min.val)
  cells.to.modify2 <- sample(1:length(vector[cells.to.modify]), amount2, replace = F)
  vector[cells.to.modify2] <- rtnorm(amount2, lower = max.val, upper = Inf)
  return(vector)
}
#Function to replace NAs by outliers in a vector
add.outlier.to.NAvector <- function(vector){
v.new <- vector
indices <- which(is.na(vector))
n <- length(indices)
m <- mean(vector[-indices])
s <- sd(vector[-indices])
mi <- min(vector[-indices], na.rm = TRUE)
ma <- max(vector[-indices], na.rm = TRUE)
if (n <=0) {
  indices2 <- indices}else
    { indices2 <- sample(indices, n/2)
v.new[indices2] <- sample((m-8*s):(m-2*s),length(indices2), replace=T)
indices3 <- which(is.na(v.new))
v.new[indices3] <- rtnorm(length(indices3), lower = m+2*s, upper =  m+8*s)
}

  return(v.new)
}
# Function to inject outliers in a dataframe
addOutlier <- function (data, amount){
  temp <- data
  for (i in 1:ncol(data)) {
    data[,i] <- add.outlier.to.vector(data[,i], amount)
  }
  return (data)
} 
# Function to replace NAs by outliers in a dataframe
addOutsinNAs <- function (data){
  data <- as.data.frame(unlist(sapply(data, add.outlier.to.NAvector)))
  #temp <- data
  #for (i in 1:ncol(data)){
  #  data[,i] <- add.outlier.to.NAvector(data[,i])
  #}
  return(data)
}
# Function to inject missing values in a dataframe
addMissing <- function(data, amount){
  temp <- data
  amount2 <- ifelse(amount<1, (prod(dim(data))*amount), amount)
  if (amount2 >= prod(dim(data))) stop("exceeded data size")
  for (i in 1:amount2) temp[sample.int(nrow(temp), 1), sample.int(ncol(temp), 1)] <- NA
  return(temp)
}  
# Function to count number of NAs in a dataframe
na_count <- function (data){a <- sum(is.na(data))}

# Function to compare vectors and count NAs that missmatch
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  n <- 0
  for (i in 1:length(same))
    if (same[i] == "FALSE"){
      n <- n+1
    }
  return(n)
}
# Function to count same number of NAs in two vectors
cuenta <- function (v1, v2){
  same <- (is.na(v1) & is.na(v2))
  same[is.na(same)] <- TRUE
  n <- 0
  for (i in 1:length(same))
    if (same[i] == "TRUE"){
      n <- n+1
    }
  return(n)
}

# Function to get results of comparing different outlier detection methods
library(stringr)
table.detect.outs <- function (data, amount.missing, nameofDF=character, detect.method = character){
  comment(nameofDF) <- nameofDF
  df.na <- addMissing(data, amount.missing)
  num.true.out <- na_count (df.na)
  df.outs <- addOutsinNAs(df.na) 
  df.detected <- outlier(df.outs, method = detect.method)
  na.match <- sum(sapply(1:ncol(df.na), function(x) cuenta(df.na[,x], df.detected$Outlyered_Data[,x])))
  num.det.out <- na_count(df.detected$Outlyered_Data)
  prec.outs <- (na.match/(na.match + (num.det.out - na.match)))*100
  detect.rate <- (na.match/num.true.out)*100
  outs.tab <- as.data.frame(list(comment(nameofDF), detect.method, amount.missing, num.true.out, num.det.out, na.match, prec.outs, detect.rate))
  names(outs.tab) <- c("Dataset", "Detection method","Amount of outliers (%)", "Number of true Outliers", "Total Outliers detected", "Num Outliers correctly detected", "Precision", "Detection rate")
  results <- list(outs.tab, df.na, df.outs, df.detected$Outlyered_Data, num.det.out)
  names(results) <- c("TabDetectionOutliers","DFnas", "OriginalOutliers", "DetectedOutliers", "NumDetOut")
  return(results)
}

# ----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# Read all completed synthetic dataframes
# First setwd inside /R_Scripts/SyntecData
library(rstan)
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/") #need to be defined to get orignldf
originaldfs <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_originaldfs <- llply(originaldfs, read.table, header = T,  dec=".", sep= ",")  # read all files to list

n <- length(originaldfs)
det.meth <- c("iqr", "adj.quan", "pcout", "lof")
m <- length(det.meth)
# For amount.missing = 0.015
for (i in 1:n){
  for (j in 1:m){
  nameoforiginaldf = substr(originaldfs[i],start = 1,stop = str_length(originaldfs[i])-4)
  list.table.outs <- llply(list_originaldfs, table.detect.outs, amount.missing = 0.015, nameofDF = nameoforiginaldf, detect.method = det.meth[j])
  write.table(file = sprintf("outstab/%s_outs015%02d.txt", originaldfs[i],j), list.table.outs[[i]]$TabDetectionOutliers, row.names = F, sep = ",")
  write.table(file = sprintf("origoutsdf/%s_orgout015%02d.txt", originaldfs[i],j), list.table.outs[[i]]$OriginalOutliers, row.names = F, sep = ",")
  write.table(file = sprintf("detecoutdf/%s_detout015%02d.txt", originaldfs[i],j), list.table.outs[[i]]$DetectedOutliers, row.names = F, sep = ",")
    }
}


# For amount.missing = 0.025
for (i in 1:n){
  for (j in 1:m){
    nameoforiginaldf = substr(originaldfs[i],start = 1,stop = str_length(originaldfs[i])-4)
    list.table.outs <- llply(list_originaldfs, table.detect.outs, amount.missing = 0.025, nameofDF = nameoforiginaldf, detect.method = det.meth[j])
    write.table(file = sprintf("outstab/%s_outs025%02d.txt", originaldfs[i],j), list.table.outs[[i]]$TabDetectionOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("origoutsdf/%s_orgout025%02d.txt", originaldfs[i],j), list.table.outs[[i]]$OriginalOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("detecoutdf/%s_detout025%02d.txt", originaldfs[i],j), list.table.outs[[i]]$DetectedOutliers, row.names = F, sep = ",")
  }
}

# For amount.missing = 0.05

for (i in 1:n){
  for (j in 1:m){
    nameoforiginaldf = substr(originaldfs[i],start = 1,stop = str_length(originaldfs[i])-4)
    list.table.outs <- llply(list_originaldfs, table.detect.outs, amount.missing = 0.05, nameofDF = nameoforiginaldf, detect.method = det.meth[j])
    write.table(file = sprintf("outstab/%s_outs05%02d.txt", originaldfs[i],j), list.table.outs[[i]]$TabDetectionOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("origoutsdf/%s_orgout05%02d.txt", originaldfs[i],j), list.table.outs[[i]]$OriginalOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("detecoutdf/%s_detout05%02d.txt", originaldfs[i],j), list.table.outs[[i]]$DetectedOutliers, row.names = F, sep = ",")
  }
}

# for amount.missing = 0.10

for (i in 1:n){
  for (j in 1:m){
    nameoforiginaldf = substr(originaldfs[i],start = 1,stop = str_length(originaldfs[i])-4)
    list.table.outs <- llply(list_originaldfs, table.detect.outs, amount.missing = 0.10, nameofDF = nameoforiginaldf, detect.method = det.meth[j])
    write.table(file = sprintf("outstab/%s_outs10%02d.txt", originaldfs[i],j), list.table.outs[[i]]$TabDetectionOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("origoutsdf/%s_orgout10%02d.txt", originaldfs[i],j), list.table.outs[[i]]$OriginalOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("detecoutdf/%s_detout10%02d.txt", originaldfs[i],j), list.table.outs[[i]]$DetectedOutliers, row.names = F, sep = ",")
  }
}

# For amount.missing = 0.15

for (i in 1:n){
  for (j in 1:m){
    nameoforiginaldf = substr(originaldfs[i],start = 1,stop = str_length(originaldfs[i])-4)
    list.table.outs <- llply(list_originaldfs, table.detect.outs, amount.missing = 0.15, nameofDF = nameoforiginaldf, detect.method = det.meth[j])
    write.table(file = sprintf("outstab/%s_outs15%02d.txt", originaldfs[i],j), list.table.outs[[i]]$TabDetectionOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("origoutsdf/%s_orgout15%02d.txt", originaldfs[i],j), list.table.outs[[i]]$OriginalOutliers, row.names = F, sep = ",")
    write.table(file = sprintf("detecoutdf/%s_detout15%02d.txt", originaldfs[i],j), list.table.outs[[i]]$DetectedOutliers, row.names = F, sep = ",")
  }
}

## Binding all outliers tables  in one
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data/outstab")
dfs.tbouts <- dir(pattern = ".txt")
list.tbouts <- llply(dfs.tbouts, read.table, header =T, dec = ".", sep = ",")
n05 <- as.data.frame(bind_rows(list.tbouts))
write.table(format(n05, trim=T, digits=4), "TablaOuts.txt", row.names=F, quote=F, sep=",")

#library(xtable) # To extract R tables to Latex
#xtable(TablaOuts)
#............................................................................................

## Creating plots using TablaOuts.txt
# PLOTS PRECISION

# function to generate plots for Outlier precision 
plot.out.precision <- function(dat.plot, mytitle=character){
  p1 <- ggplot(data = dat.plot, aes(x= Amount.of.outliers...., y = Precision)) +  
    geom_point(aes(colour = Detection.method,
                   shape = Detection.method)) +
    geom_line(aes(colour = Detection.method)) +
    scale_colour_discrete(name="Detection\nMethod") +
    scale_shape_discrete(name="Detection\nMethod")#+
  #scale_x_continuous(breaks = round(seq(min(tab21$Amount.of.outliers....), max(tab21$Amount.of.outliers....), by = 0.03),7), labels = scales::percent) 
  #  scale_x_continuous(labels = scales::percent)
  # finished line plot
  p1 <- p1 + #labs(title = " ", x ="Outlying data rate", y = "Precision") +
    theme_bw() +
    xlab("Outlying data rate") + ylab("Precision") + labs(title=mytitle) +
    theme(plot.title = element_text(hjust =0))+
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
  
}

# function to generate plots for Outlier Detection rate 
plot.out.detection <- function(dat.plot, mytitle=character){
  p1 <- ggplot(data = dat.plot, aes(x= Amount.of.outliers...., y = Detection.rate)) +  
    geom_point(aes(colour = Detection.method,
                   shape = Detection.method)) +
    geom_line(aes(colour = Detection.method)) +
    scale_colour_discrete(name="Detection\nMethod") +
    scale_shape_discrete(name="Detection\nMethod")#+
  #scale_x_continuous(breaks = round(seq(min(tab21$Amount.of.outliers....), max(tab21$Amount.of.outliers....), by = 0.03),7), labels = scales::percent) 
  #  scale_x_continuous(labels = scales::percent)
  # finished line plot
  p1 <- p1 + 
    theme_bw() +
    xlab("Outlying data rate") + ylab("Detection rate") + labs(title=mytitle) +
    theme(plot.title = element_text(hjust =0))+
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() )+
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5))
  
}

# Function to get a shared legend in multiple plots
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}


#-------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra) # for functin arrangeGrob
library(grid) #  for function unit.c()
setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data")
TablaOuts <- read.csv("./outstab/TablaOuts2.txt") 
# Making plot for DF 21
tab21 <- TablaOuts[1:20,]
tab4mil <- TablaOuts[21:40,]
tab6cent <- TablaOuts[41:60,]

p1a <- plot.out.precision(tab21, mytitle = "N21")
p1b <- plot.out.precision(tab4mil, mytitle = "N4000")
p1c <- plot.out.precision(tab6cent, mytitle = "N600")

p2a <- plot.out.detection(tab21, mytitle = "N21")
p2b <- plot.out.detection(tab4mil, mytitle = "N4000")
p2c <- plot.out.detection(tab6cent, mytitle = "N600")



grid_arrange_shared_legend(p1a,p1b,p1c,p2a, p2b,p2c, ncol =3, nrow = 2, position = "bottom")

# Plots of Precision of oytliers detection methods
#library(cowplot)
#plot_grid(p1,p3,p2, labels=c("N21","N600", "N4000"), ncol =3, nrow=1)

#.................................................................................................
## Creating plots using TablaOuts.txt
# PLOTS Detection rate

setwd("G:/Documents/Doctorado/Tesis/Informatics/R_Scripts/SyntecData/data")
TablaOuts <- read.csv("./outstab/TablaOuts.txt") 

# Making plot for DF 21
tab21 <- TablaOuts[1:20,]
p4 <- ggplot(data = tab21, aes(x= Amount.of.outliers...., y = Detection.rate)) +  
  geom_point(aes(colour = Detection.method,
                 shape = Detection.method)) +
  geom_line(aes(colour = Detection.method)) +
  scale_colour_discrete(name="Detection\nMethod") +
  scale_shape_discrete(name="Detection\nMethod")#+
#scale_x_continuous(breaks = round(seq(min(tab21$Amount.of.outliers....), max(tab21$Amount.of.outliers....), by = 0.03),7), labels = scales::percent) 
#  scale_x_continuous(labels = scales::percent)
# finished line plot
p4 <- p4 + labs(title = " ", x ="Outlying data rate", y = "Detection rate") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# Making plot for DF 4000
tab4mil <- TablaOuts[21:40,]
p5 <- ggplot(data = tab4mil, aes(x= Amount.of.outliers...., y = Detection.rate)) +  
  geom_point(aes(colour = Detection.method,
                 shape = Detection.method)) +
  geom_line(aes(colour = Detection.method)) +
  scale_colour_discrete(name="Detection\nMethod") +
  scale_shape_discrete(name="Detection\nMethod")#+
#scale_x_continuous(breaks = round(seq(min(tab21$Amount.of.outliers....), max(tab21$Amount.of.outliers....), by = 0.03),7), labels = scales::percent) 
#  scale_x_continuous(labels = scales::percent)
# finished line plot
p5 <- p5 + labs(title = " ", x ="Outlying data rate", y = "Detection rate") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# Making plot for DF 600
tab6cent <- TablaOuts[41:60,]
p6 <- ggplot(data = tab6cent, aes(x= Amount.of.outliers...., y = Detection.rate)) +  
  geom_point(aes(colour = Detection.method,
                 shape = Detection.method)) +
  geom_line(aes(colour = Detection.method)) +
  scale_colour_discrete(name="Detection\nMethod") +
  scale_shape_discrete(name="Detection\nMethod")#+
#scale_x_continuous(breaks = round(seq(min(tab21$Amount.of.outliers....), max(tab21$Amount.of.outliers....), by = 0.03),7), labels = scales::percent) 
#  scale_x_continuous(labels = scales::percent)
# finished line plot
p6 <- p6 + labs(title = " ", x ="Outlying data rate", y = "Detection rate") +
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# Plots of Precision of oytliers detection methods
library(cowplot)
plot_grid(p4,p6,p5, labels=c("N21","N600", "N4000"), ncol =3, nrow=1)

#.......................................................................................






















#*****************************************************************************************
##########################################################################################

# Datos creados en: No usados para mis experimentos (archivo eliminado el 04 Octubre 2016)
# 27th May 2016

# Injection of outliered values

# read all files to list
dataframes <- dir( pattern = ".txt") # gets a vector of all txt dataframes saved on the data file
list_dataframes <- llply(dataframes, read.table, header = T,  dec=".", sep= ",")  # read all files to list
n <- length(dataframes)

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.05) # inject 5% outlier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS05n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.10) # inject 10% outlier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS10n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.15) # inject 15% outlier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS15n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.20) # inject 20% outlier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS20n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.25) # inject 25% outlier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS25n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}

#-------------

for (j in 1:10){
  modified_list <- llply(list_dataframes, addOutlier, 0.30) # inject 30% oultier values
  for (i in 1:n)
    write.table(file = sprintf( "outliered/%s_OUTS30n%02d.txt", dataframes[i], j), modified_list[[i]], row.names = F, sep=",")
}


#--------------
