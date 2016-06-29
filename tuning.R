# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
run <- args[1]
run <- ifelse(is.na(run), 1, run)

# set the random seed, held constant for the current run
seeds <- readLines("seeds.txt")
seed <- ifelse(length(seeds[run]) == 0, sample(1:1000, 1), seeds[as.integer(run)])
set.seed(seed)

# saves that script start time
date_time <- ifelse(is.na(args[2]), format(Sys.time(), "%Y-%m-%d_%H.%M"), args[2])

# creates current output directory for current execution
output_dir <- paste("output", date_time, sep="/")
if(!dir.exists(output_dir))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

# these params always exist if launched by the bash script run-tuning.sh
models_file <- ifelse(is.na(args[3]), "models/models1.txt", args[3])
csv_file <- ifelse(is.na(args[4]), "input/test.csv", args[4])

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

# enables multicore parallel processing on unix-like systems only
#if(.Platform$OS.type != "windows") {
#  library(doMC)
#  library(parallel)
  # reads the number of cores
#  c <- detectCores()
#  registerDoMC(cores = c)
#} else {
#  print("Multicore parellel processing not available on Winodws")
#}

# comma delimiter
SO <- read.csv(csv_file, header = TRUE, sep=",")

# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("answer_uid", "upvotes", "upvotes_rank")
SO <- SO[ , !(names(SO) %in% excluded_predictors)]
predictorsNames <- names(SO[,!(names(SO)  %in% c(outcomeName))]) # removes the var to be predicted from the test set

# convert boolean factors 
SO$has_links<- as.integer(as.logical(SO$has_links))

# first check whether thera are leading and trailing apostrophes around the date_time field
SO$date_time <- gsub("'", '', SO$date_time)
# then convert timestamps into POSIX std time values, then to equivalent numbers
SO$date_time <- as.numeric(as.POSIXct(strptime(SO$date_time, tz="CET", "%Y-%m-%d %H:%M:%S")))

# normality adjustments for indipendent vars (predictors)
# ln(x+1) transformation mitigates skeweness
for (i in 1:length(predictorsNames)){
  SO[,predictorsNames[i]] <- log1p(SO[,predictorsNames[i]])
}
# exclude rows with Na, NaN and Inf (missing values)
SO <- na.omit(SO)
#SO <- SO[complete.cases(SO), ]

# create stratified training and test sets from SO dataset
splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE)
training <- SO[splitIndex, ]
testing <- SO[-splitIndex, ]

# 10-fold CV repetitions
fitControl <- trainControl(
  method = "cv",
  number = 10,
  ## repeated ten times, works only with method="repeatedcv"
  repeats = 10,
  #verboseIter = TRUE,
  #savePredictions = TRUE,
  # binary problem
  summaryFunction=twoClassSummary,
  classProbs = TRUE,
  # enable parallel computing if avail
  allowParallel = TRUE,
  returnData = FALSE
)

# load all the classifiers to tune
classifiers <- readLines(models_file)

for(i in 1:length(classifiers)){
  nline <- strsplit(classifiers[i], ":")[[1]]
  classifier <- nline[1]
  cpackage <- nline[2]
  print(paste("Building model for classifier", classifier))

  if(classifier == "gamboost") {
    ## quick fix, has_links predictor causes error
    predictorsNames <- names(SO[,!(names(SO)  %in% c("has_links"))]) 
    SO <- SO[ , !(names(SO) %in% c("has_links"))]
    training <- training[ , !(names(training) %in% c("has_links"))]
    testing <- testing[ , !(names(testing) %in% c("has_links"))]
  } 
  
  if(classifier == "xgbTree") {
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
                          method = classifier,
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
                          method = classifier,
                          trControl = fitControl,
                          metric = "ROC",
                          tuneLength = 5 # five values per param
    )
    time.end <- Sys.time()
  }
  
  # output file for the classifier at nad
  output_file <- paste(output_dir, paste(classifier, "txt", sep="."), sep = "/")
  
  cat("", "===============================\n", file=output_file, sep="\n", append=TRUE)
  cat("Seed:", seed, file=output_file, sep="\n", append=TRUE)
  out <- capture.output(model)
  title = paste(classifier, run, sep = "_run# ")
  cat(title, out, file=output_file, sep="\n", append=TRUE)

  # elapsed time
  time.elapsed <- time.end - time.start
  out <- capture.output(time.elapsed)
  cat("\nElapsed time", out, file=output_file, sep="\n", append=TRUE)
  
  # the highest roc val from train to save
  out <- capture.output(getTrainPerf(model))
  cat("\nHighest ROC value:", out, file=output_file, sep="\n", append=TRUE)
  
  #predictions <- predict(object=model, testing[,predictorsNames], type='prob')
  #head(predictions)
  #auc <- roc(ifelse(testing[,outcomeName]=="True",1,0), predictions[[2]])
  #out <- capture.output(auc$auc)
  #cat("", out, file=paste(classifier, "txt", sep="."), sep="\n", append=TRUE)
  
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
