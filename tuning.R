# enables commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
run <- args[1]
run <- ifelse(is.na(run),-1, run)

# logs errors to file
log.error <- function() {
  cat(geterrmessage(), file=paste(format(Sys.time(), "%d-%m-%Y-%X"), log, sep = "."), append=TRUE)
}
options("error"=log.error)

# library setup, depedencies are handled by R
library(e1071) # for normality adjustment
#library(pROC) # for AUC
library(caret) # for param tuning

# comma delimiter
#SO <- read.csv("so_features.csv", header = TRUE)
SO <- read.csv("head.csv", header = TRUE)

# name of outcome var to be predicted
outcomeName <- 'solution'
# list of predictor vars by name
predictorsNames <- names(SO[,  !(names(SO)  %in% c(outcomeName))]) # removes the var to be predicted from the test set

# converts boolean factors 
SO$has_links<- as.logical.factor(SO$has_links)

# first converts timestamps into POSIX std time values
SO$date_time <- as.numeric(as.POSIXct(SO$date_time, tz = "GMT", format = "'%Y-%m-%d %H:%M:%S'")) # then to equivalent number

# normality adjustments for indipendent vars (predictors)
# ln(x+1) transformation mitigates skeweness
for (i in 1:length(predictorsNames)){
  SO[,predictorsNames[i]] <- log1p(SO[,predictorsNames[i]])
}
# excludes rows with NaN (missing values)
SO <- na.omit(SO)

# SO$answers_count <- log1p(SO$answers_count)
# SO$time_difference <- log1p(SO$time_difference)
# SO$time_difference_rank <- log1p(SO$time_difference_rank)
# SO$len <- log1p(SO$len)
# SO$len_rank <- log1p(SO$len_rank)
# SO$wordcount <- log1p(SO$wordcount)
# SO$wordcount_rank <- log1p(SO$wordcount_rank)
# SO$avg_chars_per_word <- log1p(SO$avg_chars_per_word)
# SO$avg_chars_per_word_rank <- log1p(SO$avg_chars_per_word_rank)
# SO$sentences <- log1p(SO$sentences)
# SO$sentences_rank <- log1p(SO$sentences_rank)
# SO$avg_words_per_sentence <- log1p(SO$avg_words_per_sentence)
# SO$avg_words_per_sentence_rank <- log1p(SO$avg_words_per_sentence_rank)
# SO$longest_sentence <- log1p(SO$longest_sentence)
# SO$longest_sentence_rank <- log1p(SO$longest_sentence_rank)
# SO$loglikelihood <- log1p(SO$loglikelihood)
# SO$loglikelihood_ascending_rank <- log1p(SO$loglikelihood_ascending_rank)
# SO$F.K <- log1p(SO$F.K)
# SO$F.K_ascending_rank <- log1p(SO$F.K_ascending_rank)
# SO$has_links <- log1p(SO$has_links)

# sets the fixed random seed for this run
set.seed(sample(1:1000, 1))
#set.seed(45645)

# create stratified training and test sets from SO dataset
splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE, times = 1)
training <- SO[splitIndex, ]
testing <- SO[-splitIndex, ]

# 10 repetitions
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10,
                           # binary problem
                           summaryFunction=twoClassSummary,
                           classProbs = TRUE,
                           # enable parallel computing if avail
                           allowParallel = TRUE)

# loads all the classifier to tune
classifier <- readLines("models1.txt")

for(i in 1:length(classifier)){
  print(paste("Building model for classifier", classifier[i]))
  
  model <- train(solution ~ ., data = training,
                 method = classifier[i],
                 trControl = fitControl,
                 metric = "ROC",
                 tuneLength = 5 # five values per param
                 )
  
  cat("", "===============================\n", file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  out <- capture.output(model)
  title = paste(classifier[i], 1, sep = "_run# ")
  cat(title, out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # the highest roc val from train to save
  out <- capture.output(getTrainPerf(model))
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # ci sono classificatori che vogliono prob e alcuni raw????
  predictions <- predict(object=model, testing[,predictorsNames], type='prob', na.action = na.pass)
  #head(predictions)
  #auc <- roc(ifelse(testing[,outcomeName]=="True",1,0), predictions[[2]])
  #out <- capture.output(auc$auc)
  #cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  
  # computes the scalar metrics
  predictions <- predict(object=model, testing[,predictorsNames], type='raw')
  CM <- table(data=predictions, reference=testing[,outcomeName])
  if(!is.atomic(CM))
    CM <- CM$table
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
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  M <- ((TP*TN) - (FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  out <- paste("Matthews phi =", M)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  B <- 1 - (sqrt((0-FPr)^2 +(1-TPr)^2)/sqrt(2))
  out <- paste("Balance =", B)
  cat("", out, file=paste(classifier[i], "txt", sep="."), sep="\n", append=TRUE)
  #rm(model)
  #rm(predictions)
  #gc()
}

# unload a package:
## detach("package:vegan", unload=TRUE)
# SO[!complete.cases(SO),]