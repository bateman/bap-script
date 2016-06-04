# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
run <- args[1]
run <- ifelse(is.na(run),-1, run)

# logs errors to file
# FIXME
log.error <- function() {
  cat(geterrmessage(), file=paste(format(Sys.time(), "%d-%m-%Y-%X"), log, sep = "."), append=TRUE)
}
options("error"=log.error)

# build a weka classifier from WPM
make_Weka_classifier <- function(classifier) {
  model <- NaN
  require("RWeka")
  if(classifier == "ADT") {
    WPM("install-package", "alternatingDecisionTrees")
    WPM("load-package", "alternatingDecisionTrees")
    ADT <- make_Weka_classifier("weka/classifiers/trees/ADTree")
    model <- ADT(solution ~ ., data = training)
  }
  
  return(model)
}

# library setup, depedencies are handled by R
#library(pROC) # for AUC
library(caret) # for param tuning
library(e1071) # for normality adjustment

# comma delimiter
#SO <- read.csv("so_features.csv", header = TRUE)
SO <- read.csv("head.csv", header = TRUE)

# name of outcome var to be predicted
outcomeName <- 'solution'
# list of predictor vars by name
predictorsNames <- names(SO[,!(names(SO)  %in% c(outcomeName))]) # removes the var to be predicted from the test set

# convert boolean factors 
SO$has_links<- as.logical.factor(SO$has_links)

# first convert timestamps into POSIX std time values
SO$date_time <- as.numeric(as.POSIXct(SO$date_time, tz = "GMT", format = "'%Y-%m-%d %H:%M:%S'")) # then to equivalent number

# normality adjustments for indipendent vars (predictors)
# ln(x+1) transformation mitigates skeweness
for (i in 1:length(predictorsNames)){
  SO[,predictorsNames[i]] <- log1p(SO[,predictorsNames[i]])
}
# exclude rows with NaN (missing values)
SO <- na.omit(SO)

# for(j in 1:10) { ... # 10 repetions
# sets the fixed random seed for this run
#set.seed(sample(1:1000, 1))
set.seed(45645)

# create stratified training and test sets from SO dataset
splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE, times = 1)
training <- SO[splitIndex, ]
testing <- SO[-splitIndex, ]

# 10 repetitions
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 2,
  # binary problem
  summaryFunction=twoClassSummary,
  classProbs = TRUE,
  # enable parallel computing if avail
  allowParallel = TRUE)

# load all the classifier to tune
nline <- readLines("models1.txt")
nline <- strsplit(nline, ":")[[1]]
classifier <- nline[[1]]
cpackage <- nline[2]

for(i in 1:length(classifier)){
  print(paste("Building model for classifier", classifier[i]))
  
  if(classifier[i] == "xgbTree") {
    xgb_grid <- expand.grid(nrounds = c(50, 100, 150, 200, 250),
                            eta = c(0.1, 0.2, 0.3, 0.4, 0.5),
                            max_depth = c(1, 2, 3, 4, 5),
                            # defaults, untuned
                            gamma = 0,
                            colsample_bytree = 1,
                            min_child_weight = 1
                           )
    time.start <- Sys.time()
    model <- caret::train(solution ~ ., 
                          data = training,
                          method = classifier[i],
                          trControl = fitControl,
                          tuneGrid = xgb_grid,
                          metric = "ROC"
                          )
    time.end <- Sys.time()
  } 
  else if(classifier[i] == "gamboost") {
    gamboost_grid <- expand.grid(mstop = c(50, 100, 150, 200, 250),
                                 prune = c(1,2,3,4,5)
                                )
    time.start <- Sys.time()
    model <- caret::train(solution ~ ., 
                          data = training,
                          method = classifier[i],
                          trControl = fitControl,
                          tuneGrid = gamboost_grid,
                          metric = "ROC"
    )
    time.end <- Sys.time()
  } 
  else if(cpackage[i] == "WPM") {
    model = make_Weka_classifier(classifier[i])
  } 
  else {
    time.start <- Sys.time()
    model <- caret::train(solution ~ ., 
                          data = training,
                          method = classifier[i],
                          trControl = fitControl,
                          metric = "ROC",
                          tuneLength = 2 # five values per param
                          )
    time.end <- Sys.time()
  }
  cat("", "===============================\n", file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)

  out <- capture.output(model)
  title = paste(classifier[i], 1, sep = "_run# ")
  cat(title, out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # elapsed time
  time.elapsed <- time.end - time.start
  out <- capture.output(time.elapsed)
  cat("\nElapsed time", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # the highest roc val from train to save
  out <- capture.output(getTrainPerf(model))
  cat("\nHighest ROC value:", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  predictions <- predict(object=model, testing[,predictorsNames], type='prob')
  #head(predictions)
  #auc <- roc(ifelse(testing[,outcomeName]=="True",1,0), predictions[[2]])
  #out <- capture.output(auc$auc)
  #cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # computes the scalar metrics
  predictions <- predict(object=model, testing[,predictorsNames], type='raw')
  CM <- table(data=predictions, reference=testing[,outcomeName])
  out <- capture.output(CM)
  cat("\nConfusion Matrix:\n", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  TP <- CM[1]
  FP <- CM[3]
  FN <- CM[2]
  TN <- CM[4]
  precision <- posPredValue(predictions, testing[,outcomeName])
  recall <- sensitivity(predictions, testing[,outcomeName])
  TNr <- specificity(predictions, testing[,outcomeName])
  TPr <- recall
  FPr <- FP / (FP + TN)
  
  F1 <- (2 * precision * recall) / (precision + recall)
  out <- paste("F-measure =", F1)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  G <- sqrt(TPr * TNr)
  out <- paste("G-mean =", G)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep = "\n", append = TRUE)
  M <- ((TP*TN) - (FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  out <- paste("Matthews phi =", M)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep = "\n", append = TRUE)
  B <- 1 - (sqrt((0-FPr)^2 +(1-TPr)^2)/sqrt(2))
  out <- paste("Balance =", B)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep = "\n", append = TRUE)
  
  ## === cleanup ===
  # deallocate large objects
  rm(model)
  rm(predictions)
  # unload the package:
  if(!is.na(cpackage))
    detach(name=paste("package", cpackage, sep=":"), unload = TRUE, character.only = TRUE)
  # garbage collection
  gc()
}


# } # end of for 10 repetitions

# SO[!complete.cases(SO),]