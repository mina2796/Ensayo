library(som)
library(dprep)

decscale <- function (data) {
  d = dim(data)
  c = class(data)
  cnames = colnames(data)
  #classes = data[, 1] # todo en pausa, original: classes = data[, d[2]]
  #data = data [, -1]# todo en pausa, original: data = data [, -d[2]]
  maxvect = apply(abs(data), 2, max, na.rm=T)
  kvector = ceiling(log10(maxvect))
  scalefactor = 10^kvector
  decdata = scale(data, center = FALSE, scale = scalefactor)
  attributes(decdata) = NULL
  decdata = matrix(decdata, dim(data)[1], dim(data)[2])
  #decdata = cbind(classes, decdata) # todo en pausa, original: decdata = cbind(decdata, classes)
  if (c == "data.frame") 
    decdata = as.data.frame(decdata)
  colnames(decdata) = cnames
  return(decdata)
}
mmnorm <- function (data,minval=0,maxval=1){                                                                               
  d=dim(data)                                                                     
  c=class(data)                                                                   
  cnames=colnames(data)                                                           
  # classes=data[,d[2]]                                                             
  # data=data[,-d[2]]                                                               
  minvect=apply(data,2,min, na.rm=T)
  maxvect=apply(data,2,max, na.rm=T)                                                       
  rangevect=maxvect-minvect                                                      
  zdata=scale(data,center=minvect,scale=rangevect)                                
  newminvect=rep(minval,d[2])                                                   
  newmaxvect=rep(maxval,d[2])                                                  
  newrangevect=newmaxvect-newminvect                                             
  zdata2=scale(zdata,center=FALSE,scale=(1/newrangevect))                        
  zdata3=zdata2+newminvect                                                      
  #zdata3=cbind(zdata3,classes)                                                    
  if (c=="data.frame") zdata3=as.data.frame(zdata3)                             
  colnames(zdata3)=cnames                                                         
  return(zdata3)                                                                  
}
  
signorm <- function (data){
    d = dim(data)
    c = class(data)
    cnames = colnames(data)
    classes = data[, 1] # estaba en pausa: classes = data[, d[2]]
    zdata = znorm(data)
    d2 = dim(zdata)
    zdata = zdata[, -1] # estaba en pausa solo lo de los corchetes: zdata = zdata[, -d2[2]]
    sigdata = (1 - exp(-zdata))/(1 + exp(-zdata))
    sigdata = cbind( classes, sigdata) # estaba en pausa: sigdata = cbind(sigdata, classes)
    if (c == "data.frame") 
      sigdata = as.data.frame(sigdata)
    colnames(sigdata) = cnames
    return(sigdata)
}
znorm <- function (data){
  d = dim(data)
  c = class(data)
  cnames = colnames(data)
 # classes = data[, d[2]]
 # data = data[, -d[2]]
  zdata = scale(data)
  attributes(zdata) = NULL
  zdata = matrix(zdata, dim(data)[1], dim(data)[2])
 # zdata = cbind(zdata, classes)
  if (c == "data.frame") 
    zdata = as.data.frame(zdata)
  colnames(zdata) = cnames
  return(zdata)
}
softmaxnorm <- function (data){
    d = dim(data)
    c = class(data)
    cnames = colnames(data)
    classes = data[, 1] # todo en pausa:  classes = data[, d[2]]
    zdata = znorm(data)
    d2 = dim(zdata)
    zdata = zdata[, -1] # todo en pausa: zdata = zdata[, -d2[2]]
    softdata = 1/(1 + exp(-zdata))
    softdata = cbind(classes, softdata) # todo en pausa: softdata = cbind(softdata, classes)
    if (c == "data.frame") 
      softdata = as.data.frame(softdata)
    colnames(softdata) = cnames
    return(softdata)
}



Normalization <- function(data, type = c("NormZerOne", "decscale", "mmnorm", "signorm", "softmaxnorm", "znorm"), Normalization = TRUE){
  if (!is.data.frame(data) && !is.matrix(data) && !is.numeric(data)) stop(warning('Input must be one of classes \"vector\", \"data frame\" or \"matrix\"'))
 # na.fail (data)
  # if (is.na(data) == TRUE || is.null(dim(data))) { stop('not missing values allowed')}
  
  #  if (dim(data)[1] < 2 || is.null(dim(data))) { stop ("number of values must be greater than 2")} This condition must be performed
  type = match.arg(type)
  
  
  if (type == "NormZerOne") TestName = "NormZerOne"
#  if (type == "NormLog") TestName = "NormLog"
  if (type == "decscale") TestName = "Decimal scale normalization"
  if (type == "mmnorm") TestName = "Min-Max normalization"
  if (type == "signorm") TestName = "signorm"
  if (type == "softmaxnorm") TestName = "softmaxnorm"
  if (type == "znorm") TestName = "Z-score normalization"
  
  if (is.data.frame(data) || is.matrix(data)){ 
    if (type == "NormZerOne"){
      normalized_ZerOne <- normalize(data, byrow = FALSE)
      normalizedDF <- normalized_ZerOne
    }else
    
    if (type == "decscale"){
      normalized_decscale <- decscale(data)
      normalizedDF <- normalized_decscale
    }else
    
    if (type == "mmnorm"){
      normalized_mmnorm <- mmnorm(data, minval=0, maxval=1)
      normalizedDF <- normalized_mmnorm
    }else
    
    if (type == "signorm"){
      normalized_signorm <- signorm(data)
      normalizedDF <- normalized_signorm
    }else
    
    if (type == "softmaxnorm"){
      normalized_softmaxnorm <- softmaxnorm(data)
      normalizedDF <- normalized_softmaxnorm
    }else
    
    if (type == "znorm"){
      normalized_znorm <- znorm(data)
      normalizedDF <- normalized_znorm
    }
  }
  if (is.matrix(data)){
    data = data.frame(data)
  }

  return (normalizedDF)
    
  }
