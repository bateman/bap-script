# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
run <- args[1]
run <- ifelse(is.na(run), 0, run)

# set the random seed, held constant for the current run
seeds <- readLines("seeds.txt")
seed <- ifelse(length(seeds[run]) == 0, sample(1:1000, 1), seeds[as.integer(run)])

# saves that script start time
date_time <- ifelse(is.na(args[2]), format(Sys.time(), "%Y-%m-%d_%H.%M"), args[2])

# creates current output directory for current execution
output_dir <- paste("output", date_time, sep="/")
if(!dir.exists(output_dir))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

# logs errors to file
 error_file <- paste(date_time, "log", sep = ".")
log.error <- function() {
  cat(geterrmessage(), file=paste(output_dir, error_file, sep = "/"), append=TRUE)
}
options(show.error.locations=TRUE)
options(error=log.error)


# library setup, depedencies are handled by R
#library(pROC) # for AUC
library(caret) # for param tuning
library(e1071) # for normality adjustment

# comma delimiter
#SO <- read.csv("input/so_features.csv", header = TRUE)
SO <- read.csv("input/head.csv", header = TRUE, sep=",")

# name of outcome var to be predicted
outcomeName <- 'solution'
# list of predictor vars by name
predictorsNames <- names(SO[,!(names(SO)  %in% c(outcomeName))]) # removes the var to be predicted from the test set

# convert boolean factors 
SO$has_links<- as.integer(as.logical(SO$has_links))

# first convert timestamps into POSIX std time values
SO$date_time <- as.numeric(as.POSIXct(SO$date_time, tz = "GMT", format = "'%Y-%m-%d %H:%M:%S'")) # then to equivalent number

# normality adjustments for indipendent vars (predictors)
# ln(x+1) transformation mitigates skeweness
for (i in 1:length(predictorsNames)){
  SO[,predictorsNames[i]] <- log1p(SO[,predictorsNames[i]])
}
# exclude rows with NaN (missing values)
SO <- na.omit(SO)

# create stratified training and test sets from SO dataset
splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE, times = 1)
training <- SO[splitIndex, ]
testing <- SO[-splitIndex, ]

# 10-fold CV repetitions
fitControl <- trainControl(## 
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 2,
  # binary problem
  summaryFunction=twoClassSummary,
  classProbs = TRUE,
  # enable parallel computing if avail
  allowParallel = TRUE,
  returnData = FALSE,
  returnResamp = "all"
)

# load all the classifier to tune
nline <- readLines("models1.txt")
nline <- strsplit(nline, ":")[[1]]
classifier <- nline[[1]]
cpackage <- nline[2]

for(i in 1:length(classifier)){
  print(paste("Building model for classifier", classifier[i]))

  if(classifier[i] == "gamboost") {
    ## quick fix, has_links predictor causes error
    predictorsNames <- names(SO[,!(names(SO)  %in% c("has_links"))]) 
    SO <- SO[ , !(names(SO) %in% c("has_links"))]
    training <- training[ , !(names(training) %in% c("has_links"))]
    testing <- testing[ , !(names(testing) %in% c("has_links"))]
  } 
  
  if(classifier[i] == "xgbTree") {
    xgb_grid <- expand.grid(nrounds = c(50, 100, 150, 200, 250),
                            eta = c(0.1, 0.3, 0.5, 0.7),
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
  
  # output file for the classifier at nad
  output_file <- paste(output_dir, paste(classifier[i], "txt", sep="."), sep = "/")
  
  cat("", "===============================\n", file=output_file, sep="\n", append=TRUE)
  cat("Seed:", seed, file=output_file, sep="\n", append=TRUE)
  out <- capture.output(model)
  title = paste(classifier[i], run, sep = "_run# ")
  cat(title, out, file=output_file, sep="\n", append=TRUE)

  # elapsed time
  time.elapsed <- time.end - time.start
  out <- capture.output(time.elapsed)
  cat("\nElapsed time", out, file=output_file, sep="\n", append=TRUE)
  
  # the highest roc val from train to save
  out <- capture.output(getTrainPerf(model))
  cat("\nHighest ROC value:", out, file=output_file, sep="\n", append=TRUE)
  
  predictions <- predict(object=model, testing[,predictorsNames], type='prob')
  #head(predictions)
  #auc <- roc(ifelse(testing[,outcomeName]=="True",1,0), predictions[[2]])
  #out <- capture.output(auc$auc)
  #cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # computes the scalar metrics
  predictions <- predict(object=model, testing[,predictorsNames], type='raw')
  CM <- table(data=predictions, reference=testing[,outcomeName])
  out <- capture.output(CM)
  cat("\nConfusion Matrix:\n", out, file=output_file, sep="\n", append=TRUE)
  
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
  cat("", out, file=output_file, sep="\n", append=TRUE)
  G <- sqrt(TPr * TNr)
  out <- paste("G-mean =", G)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
  M <- ((TP*TN) - (FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  out <- paste("Matthews phi =", M)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
  B <- 1 - (sqrt((0-FPr)^2 +(1-TPr)^2)/sqrt(2))
  out <- paste("Balance =", B)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
  
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


# SO[!complete.cases(SO),]