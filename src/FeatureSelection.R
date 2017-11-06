library(mlbench)
library(caret)
library(reshape2)
set.seed(7)

# function to fing High correlated variables
mosthighlycorrelated <- function(df, numtoreport){
  cormatrix <- cor(df)
  upperTriangle <- upper.tri(cormatrix, diag = F)
  cor.upperTriangle <- cormatrix
  cor.upperTriangle[!upperTriangle] <- NA
  cor.melted <- na.omit(melt(cor.upperTriangle, value.name = "CorrelationCoef"))
  colnames(cor.melted) <- c("Firts Variable", "Second Variable", "Correlation")
  head(cor.melted[order(abs(cor.melted$Correlation), decreasing=T),], n=numtoreport)
}

#--------------------- Functions for RankImportance ------------------
# Function to apply the Rank Feature by Importance
rfifunction <- function (df, control, indepVariable = character){
  longdata <- ncol(df)
  control <- trainControl(method = "boot", number = 10, repeats = 3)   # prepare training set
  f <- as.formula(paste(indepVariable, "~", "."))
  model <- train(f, data = df, method = "knn", preProcess = "scale", trControl = control)  # define model
  importance <- varImp(model, scale = FALSE)
  return(importance)
}

#---------------------- Functions for RFE ----------------------------
# Function to apply the rfe function on a dataframe
rfefunction <- function (df){                                                       
    longdata <- ncol(df)                                               
    newControl <- rfeControl(functions=caretFuncs, method = "boot", number = 3)                 # define the control 
    rfe ( df[,1:longdata], df[,longdata], sizes = c(1:longdata), rfeControl = newControl)   # apply the rfe function for feature selection.
}

# Function to get the best variables of a rfe Object
rfebestvariables <- function (rfeObject, numOfVariables){
  selectedVars <- rfeObject$variables
  bestvar <- rfeObject$control$functions$selectVar(selectedVars, numOfVariables)
  return (bestvar)
}

#------------------------------------------------
FeatureSelection <- function(data, type = c("HighCorr", "RankImp", "RFE", "Wrapper"), numvarstoreport, indepVariable = character)
  {
  if (!is.data.frame(data) && !is.matrix(data) && !is.numeric(data)) stop(warning('Input must be one of classes \"vector\", \"data frame\" or \"matrix\"'))
  type = match.arg(type)
  
  if (type == "HighCorr") TestName = "HighCorrelated"
  if (type == "RankImp") TestName = "RankImportance"
  if (type == "RFE") TestName = "Recursive Feature Elimination"
  if (type == "Wrapper") TestName = "Wrapper method"

  if (is.data.frame(data) || is.matrix(data)){ 
    varNames = colnames(data)
    dims = dim(data)
    
    if (type == "HighCorr"){   
      require(caret)
      n <- numvarstoreport
      highlyCorrelated <- mosthighlycorrelated(data, n) 
      SelectedFeatures <- highlyCorrelated
      c <- cor(data)
      f <- findCorrelation(c, cutoff = 0.6) # It finds columns with correlation >= 0.6
      new_data <- data[, -c(f)]
    }else 
    
    if (type == "RankImp"){
      #rankImp <- rfifunction(data, indepVariable = indepVariable )
      #SelectedFeatures <- rankImp
      require(mlr)
      modeled.task <- makeRegrTask(data=data, target=indepVariable)
      fv <- generateFilterValuesData(modeled.task, method="linear.correlation")
      ff <- fv$data$name[fv$data$linear.correlation > 0.3] # Esto se usa para dfN21
      ff <- append(indepVariable, ff)
      new_data <- data[ff]
      
    }else
      
    if (type == "RFE"){
      m <- numvarstoreport
      recursiveFE <- rfefunction(data)
      predictors(recursiveFE)
      bestVariables <- as.data.frame(rfebestvariables(recursiveFE, m ))
      names(bestVariables) <- c("Variable")
      SelectedFeatures <- bestVariables
      new_data <- data[, c(rfebestvariables(recursiveFE, m))]
    }else
    if (type == "Wrapper"){
      require (mlr)
      ctrl = makeFeatSelControlRandom(maxit = 20L)
      #ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 0.02) # Specify the search strategy
      modeled.task <- makeRegrTask(data=data, target=indepVariable) # fir a saimple regression model 
      rdesc = makeResampleDesc("CV", iters = 10)
      sfeats = selectFeatures(learner = "regr.lm", task = modeled.task, resampling = rdesc, control = ctrl,
                              show.info = FALSE) # Select features
      cc <- append(indepVariable, sfeats$x)
      SelectedFeatures <- sfeats
      new_data <- data[, cc]
    }
  }
  {
    return(new_data)
    #return (list(Features=SelectedFeatures, ReducedDF = new_data))
    
  }
}

#--------------------------

