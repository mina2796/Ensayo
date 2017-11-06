library(mvoutlier)
library(robustbase)
library(dprep)
library(outliers)
library(kmodR)
library(Rlof)
# Function to detect outliers with IQR in a vector
IQROutliers <- function (vector){
  lowerq <- quantile(vector)[2]
  upperq <- quantile(vector)[4]
  iqr <- upperq - lowerq
  extreme.threshold.upper <- (iqr * 3) + upperq
  extreme.threshold.lower <- lowerq - (iqr * 3)
  result <- which(vector > extreme.threshold.upper | vector < extreme.threshold.lower)
  result2 <- replace(vector, result, NA)
  return(result2)
}
# Function to detect outliers with IQR in dataframe
IQR.Outliers.DF <- function(data){
    iqr.data <- as.data.frame(unlist(sapply(data, IQROutliers)))
  return(iqr.data)
}
# Function to count number of NAs in a dataframe
na_count <- function (data){a <- sum(is.na(data))}

# Function to detect outliers with Grubbs test in a vector
grubbs.flag <- function(vector) {
  outliers <- NULL
  test <- vector
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  # throw an error if there are too few values for the Grubb's test
  if (length(test) < 3 ) stop("Grubb's test requires > 2 input values")
  na.vector <- test
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- vector[!vector %in% outliers]
    # stop if all but two values are flagged as outliers
    if (length(test) < 3 ) {
      warning("All but two values flagged as outliers")
      break
    }
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
    idx.outlier <- which(vector %in% outliers)
    na.vector <- replace(vector, idx.outlier, NA)
    #result <- list((data.frame(X=vector,Outlier=(vector %in% outliers))), na.vect)
    #names(result) <- c("outliers", "nadata")
  }
  return(na.vector)
}
# Function to detect outliers with Grubbs test in a dataframe
grubbs.df <- function(data){
  grubbs.data <- as.data.frame(unlist(sapply(data, grubbs.flag)))
  return(grubbs.data)
}

#*************************************************************************************
outlier <-
  function (data, qqplot = FALSE, alpha = 0.5, tol = 1e-25, method = c("quan", "adj.quan", "pcout","iqr", "grubbs", "lof"), label = TRUE, position = TRUE, offset = 0.8)
{
    
    if (!is.data.frame(data) && !is.matrix(data)) stop('Input must be one of classes \"data frame\" or \"matrix\"')
    
    if (dim(data)[2] < 2 || is.null(dim(data))) {stop("number of variables must be equal or greater than 2")}
    
    
    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    method <- match.arg(method)
    
    n <- dim(data)[1]
    p <- dim(data)[2]
    
   covr <- covMcd(data, alpha = alpha)
   mah <- mahalanobis(data, center = covr$center, cov = covr$cov, tol = tol)
   d <- mah
   sortMah <- data.frame(sort(mah, decreasing = TRUE)) # sorted Mahalanobis' distances in increasing order
    
    out <-  cbind(rownames(sortMah),round(sortMah,3), NA)
    colnames(out) <- c("Observation","Mahalanobis Distance", "Outlier")
    
    
    if (method=="adj.quan"){
      crt <- arw(x=data, m0=covr$center, c0=covr$cov, alpha = 0.025)$cn
      
      for(i in 1:n){
        {
          if (sortMah[i,] > crt){
            out[i,3] <- "TRUE"
          } else
          {
            out[i,3] <- "FALSE"
          }
        }
      }
      
      if (qqplot){
        d <- mah
        r <- rank(d)
        chi2q <- qchisq((r-0.5)/n,p)
        
        colors = NULL
        for (i in 1:n) {
          if (d[i] > crt) colors[i] = "red" else colors[i] = "black"
        }
        
        plot(d, chi2q , pch = 16, main = "Adjusted Chi-Square Q-Q Plot",
             xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile", col=colors)
        abline(v=crt, lwd = 2, col = "blue")
        tbl = table(out[,3])
        
        legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
               col=c("red","black"), pch=16, bty="n",)
        
        if(label && is.element("TRUE", out[, 3])) {
          labelOutlier <- rownames(out)[out[,3] == TRUE]
          xCoord <- out[out[,3] == TRUE,2]
          yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
          text(xCoord, yCoord, labelOutlier, pos = position, offset = offset)
        }
        
        if (max(d) >= crt) {text(crt-0.2,2,paste("Quantile: ", round(crt,3)), srt=90, pos=3, col="blue")}
      }
      
      newData <- out[out$Outlier %in% "FALSE",]
      ind <- sort(row.names(newData))
      newData <- data[ind,]
     idx <- which(out$Outlier %in% "TRUE")
     nodata <- as.data.frame(apply(data, 2, replace, idx, NA))
     n.out <- na_count(nodata)
     result <- list(out, newData, nodata, n.out)
     names(result) <- c("outlier", "newData", "Outlyered_Data","Num of outliers")
      
    }
    
    if (method=="quan"){
      
      chiSq <- qchisq(0.975, p)
      for(i in 1:n){
        {
          if (sortMah[i,] > chiSq){
            out[i,3] <- "TRUE"
          } else
          {
            out[i,3] <- "FALSE"
          }}
      }
      if (qqplot){
        d <- mah
        r <- rank(d)
        chi2q <- qchisq((r-0.5)/n,p)
        
        
        colors = NULL
        for (i in 1:n) {
          if (d[i] > chiSq) colors[i] = "red" else colors[i] = "black"
        }
        
        plot(d, chi2q , pch = 16, col=colors, main = "Chi-Square Q-Q Plot",
             xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile")
        abline(v=chiSq, lwd = 2, col = "blue")
        
        tbl = table(out[,3])
        
        legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
               col=c("red","black"), pch=16, bty="n",)
        
        if(label && is.element("TRUE", out[, 3])) {
          labelOutlier <- rownames(out)[out[,3] == TRUE]
          xCoord <- out[out[,3] == TRUE,2]
          yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
          text(xCoord, yCoord, labelOutlier, pos = position, offset = offset)
        }
        
        if (max(d) >= chiSq) {text(chiSq-0.2,2,paste("Quantile: ", round(chiSq,3)),srt = 90, pos = 3, col="blue")}
      }
    
      newData <- out[out$Outlier %in% "FALSE",]
      ind <- sort(row.names(newData))
      newData <- data[ind,]
      idx <- which(out$Outlier %in% "TRUE")
      nodata <- as.data.frame(apply(data, 2, replace, idx, NA))
      n.out <- na_count(nodata)
      result <- list(out, newData, nodata, n.out)
      names(result) <- c("outlier", "newData", "Outlyered_Data","Num of outliers")

    }
      
     if (method=="pcout"){
    
      pwfi <- pcout(x=data, explvar = 0.975)$wfinal01
      pwfiM1 <- pcout(x=data, explvar = 0.975)$M1
    out[,3] <- pwfi
    out[,3] <- ifelse(out[,3] == 0 , "TRUE", "FALSE")
    if (qqplot){
      d <- mah
      r <- rank(d)
      chi2q <- qchisq((r-0.5)/n,p)
      
      colors = NULL
      for (i in 1:n) {
        if (pwfi[i] == 0) colors[i] = "red" else colors[i] = "black"
      }
      
      plot(d, chi2q , pch = 16, main = "PCOut Q-Q Plot",
           xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile", col=colors)
      abline(v=pwfiM1, lwd = 2, col = "blue")
      tbl = table(out[,3])
      
      legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
             col=c("red","black"), pch=16, bty="n",)
      
      if(label && is.element("TRUE", out[, 3])) {
        labelOutlier <- rownames(out)[out[,3] == TRUE]
        xCoord <- out[out[,3] == TRUE,2]
        yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
        text(xCoord, yCoord, labelOutlier, pos = 4, offset = offset)
      }
      
      if (max(d) >= pwfiM1) {text(pwfiM1-0.2,2,paste("Quantile: ", round(pwfiM1,3)), srt=90, pos=3, col="blue")}
    }
    
    newData <- out[out$Outlier %in% "FALSE",]
    ind <- sort(row.names(newData))
    newData <- data[ind,]
    idx <- which(out$Outlier %in% "TRUE")
    nodata <- as.data.frame(apply(data, 2, replace, idx, NA))
    n.out <- na_count(nodata)
    result <- list(out, newData, nodata, n.out)
    names(result) <- c("outlier", "newData", "Outlyered_Data","Num of outliers")

   }
    if (method=="iqr"){
      pwfi <- as.data.frame(apply(data, 2, IQROutliers))
      n.out <- na_count(pwfi)

      #if (qqplot){
      #  d <- mah
      #  r <- rank(d)
      #  chi2q <- qchisq((r-0.5)/n,p)
        
      #  colors = NULL
      #  for (i in 1:n) {
      #    if (pwfi[i] == 0) colors[i] = "red" else colors[i] = "black"
      #  }
        
      #  plot(d, chi2q , pch = 16, main = "PCOut Q-Q Plot",
      #       xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile", col=colors)
      #  abline(v=pwfiM1, lwd = 2, col = "blue")
      #  tbl = table(out[,3])
        
      #  legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
      #         col=c("red","black"), pch=16, bty="n",)
        
      #  if(label && is.element("TRUE", out[, 3])) {
      #    labelOutlier <- rownames(out)[out[,3] == TRUE]
      #    xCoord <- out[out[,3] == TRUE,2]
      #    yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
      #    text(xCoord, yCoord, labelOutlier, pos = 4, offset = offset)
      #  }
        
      #  if (max(d) >= pwfiM1) {text(pwfiM1-0.2,2,paste("Quantile: ", round(pwfiM1,3)), srt=90, pos=3, col="blue")}
      #}
      
      OutsData <- pwfi
     # ind <- sort(row.names(newData))
      # newData <- data[ind,]
      
      result <- list(n.out, OutsData)
      names(result) <- c("Total number of outliers", "Outlyered_Data")
      
    }
    if (method=="grubbs"){
      pwfi <- grubbs.df(data)
      n.out <- na_count(pwfi)
      
      #if (qqplot){
      #  d <- mah
      #  r <- rank(d)
      #  chi2q <- qchisq((r-0.5)/n,p)
      
      #  colors = NULL
      #  for (i in 1:n) {
      #    if (pwfi[i] == 0) colors[i] = "red" else colors[i] = "black"
      #  }
      
      #  plot(d, chi2q , pch = 16, main = "PCOut Q-Q Plot",
      #       xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile", col=colors)
      #  abline(v=pwfiM1, lwd = 2, col = "blue")
      #  tbl = table(out[,3])
      
      #  legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
      #         col=c("red","black"), pch=16, bty="n",)
      
      #  if(label && is.element("TRUE", out[, 3])) {
      #    labelOutlier <- rownames(out)[out[,3] == TRUE]
      #    xCoord <- out[out[,3] == TRUE,2]
      #    yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
      #    text(xCoord, yCoord, labelOutlier, pos = 4, offset = offset)
      #  }
      
      #  if (max(d) >= pwfiM1) {text(pwfiM1-0.2,2,paste("Quantile: ", round(pwfiM1,3)), srt=90, pos=3, col="blue")}
      #}
      
      OutsData <- pwfi
      # ind <- sort(row.names(newData))
      # newData <- data[ind,]
      
      result <- list(n.out, OutsData)
      names(result) <- c("Total number of outliers", "Outlyered_Data")
    
    }
    if (method=="lof"){
      pwfi <- as.data.frame(lof(data, c(5:10), cores = 1))
      out2 <- as.data.frame(cbind(attr(pwfi, "row.names"), pwfi[,1], NA))
      colnames(out2) <- c("Observation", "LOF k=5", "Outlier")
      for (i in 1:nrow(pwfi)){
        if (pwfi[i,5] > 1.10){ # Este parametro dee ser definido por el usario en myapp
          out2[i,3] <- "TRUE"
        }else
        { out2[i,3] <- "FALSE"
        }
      }
      n.out <- na_count(pwfi)
      
      #if (qqplot){
      #  d <- mah
      #  r <- rank(d)
      #  chi2q <- qchisq((r-0.5)/n,p)
      
      #  colors = NULL
      #  for (i in 1:n) {
      #    if (pwfi[i] == 0) colors[i] = "red" else colors[i] = "black"
      #  }
      
      #  plot(d, chi2q , pch = 16, main = "PCOut Q-Q Plot",
      #       xlab = "Robust Squared Mahalanobis Distance",ylab="Chi-Square Quantile", col=colors)
      #  abline(v=pwfiM1, lwd = 2, col = "blue")
      #  tbl = table(out[,3])
      
      #  legend("topleft",legend=c(paste("Outliers (n=",if(is.na(tbl[2])) 0 else tbl[2],")",sep=""),paste("Non-outliers (n=",if(is.na(tbl[1])) 0 else tbl[1],")",sep="")),
      #         col=c("red","black"), pch=16, bty="n",)
      
      #  if(label && is.element("TRUE", out[, 3])) {
      #    labelOutlier <- rownames(out)[out[,3] == TRUE]
      #    xCoord <- out[out[,3] == TRUE,2]
      #    yCoord <- sort(chi2q,decreasing = T)[1:length(xCoord)]
      #    text(xCoord, yCoord, labelOutlier, pos = 4, offset = offset)
      #  }
      
      #  if (max(d) >= pwfiM1) {text(pwfiM1-0.2,2,paste("Quantile: ", round(pwfiM1,3)), srt=90, pos=3, col="blue")}
      #}
      
      newData <- out2[out2$Outlier %in% "FALSE",]
      ind <- sort(row.names(newData))
      newData <- data[ind,]
      idx <- which(out2$Outlier %in% "TRUE")
      nodata <- as.data.frame(apply(data, 2, replace, idx, NA))
      n.out <- na_count(nodata)
      result <- list(out2, newData, nodata, n.out)

      names(result) <- c("outlier","newData", "Outlyered_Data", "Num of outliers")
      
    }
    return(result)
    
  }

