# Eva c. Serran
# 11/12/2016
# Modified: 19/01/2017 Obteniendo Skweness / Kurtosis
# Data description, latex
# Mean and variance 

fs21 <- sapply(FS21, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
fs600 <- sapply(FS600, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
fs4000 <- sapply(FS4000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
fs20000 <- sapply(FS20000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))

library(xtable) # To extract R tables to Latex
xtable(fs21)

al21 <- sapply(AL21, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
al600 <- sapply(AL600, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
al4000 <- sapply(AL4000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
al20000 <- sapply(AL20000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))

library(xtable) # To extract R tables to Latex
xtable(al20000)

n21 <- sapply(N21, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
n600 <- sapply(N600, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
n4000 <- sapply(N4000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
n20000 <- sapply(N20000, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))

library(xtable) # To extract R tables to Latex
xtable(n20000)

out21 <- sapply(N21.txt_orgout1501, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
out600 <- sapply(N600.txt_orgout1501, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
out4000 <- sapply(N4000.txt_orgout1501, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
xtable(out4000)

fqmaA <- sapply(fqmaA, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
fqmaC <- sapply(fqmaC, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
fqmacrp<- sapply(fqmacro, function(cl) list(means=mean(cl,na.rm=TRUE), variance=var(cl,na.rm=TRUE)))
xtable(fqmacro)

