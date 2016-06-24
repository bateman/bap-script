# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

library(caret) # for param tuning
library(e1071) # for normality adjustment
library(ROCR)  # for plotting ROC curves

csv_file <- ifelse(is.na(args[2]), "input/test.csv", args[1])
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

set.seed(XXX)
# create stratified training and test sets from SO dataset
splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE)
training <- SO[splitIndex, ]
testing <- SO[-splitIndex, ]

# no train, model with parameters already set.
modelX <- classifier(solution ~ ., data = training, parameters...)
modelX.pred <- predict(modelX, testing)

# now plot predictions

# see http://stackoverflow.com/questions/18130338/plotting-an-roc-curve-in-glmnet
# https://mlr-org.github.io/mlr-tutorial/release/html/roc_analysis/index.html
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
pred2 <- prediction(abs(ROCR.simple$predictions +
                          rnorm(length(
                            ROCR.simple$predictions
                          ), 0, 0.1)),
                    ROCR.simple$labels)
pred3 <- prediction(abs(ROCR.simple$predictions +
                          rnorm(length(
                            ROCR.simple$predictions
                          ), 0, 0.11)),
                    ROCR.simple$labels)
perf <- performance(pred, "tpr", "fpr")
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")
par(
  mar = c(5, 5, 2, 2),
  xaxs = "i",
  #?
  yaxs = "i",
  #?
  cex.axis = 1.1,
  cex.lab = 1.2
)
g <- gray.colors(
  3,
  start = 0.2,
  end = 0.7,
  gamma = 2.2,
  alpha = NULL
)
plot(perf,
     col = g,
     lty = 1,
     lwd = 1)
plot(perf2,
     add = TRUE,
     col = g,
     lty = 2,
     lwd = 1.3
)
plot(perf3,
     add = TRUE,
     col = g,
     lty = 3,
     lwd = 1.3
)

# Add a legend
legend(
  "bottomright",
  inset = .15,
  c("1", "2", "3"),
  title = "xx",
  horiz = FALSE,
  lty = c(1:3),
  lwd = 2,
  col = g
)

abline(a = 0,
       b = 1,
       lty = "dotted",
       lwd = 0.3)
#
# # calculating the values for ROC curve
# pred <- prediction(target_pred, target_class)
# perf <- performance(pred, "tpr", "fpr")
# # changing params for the ROC plot - width, etc
# par(
#   mar = c(5, 5, 2, 2),
#   xaxs = "i",
#   yaxs = "i",
#   cex.axis = 1.3,
#   cex.lab = 1.4
# )
# plotting the ROC curve
# plot(perf,
#      col = "black",
#      lty = 3,
#      lwd = 3)
# # calculating AUC
# auc <- performance(pred, "auc")
# # now converting S4 class to vector
# auc <- unlist(slot(auc, "y.values"))
# # adding min and max ROC AUC to the center of the plot
# minauc <- min(round(auc, digits = 2))
# maxauc <- max(round(auc, digits = 2))
# minauct <- paste(c("min(AUC)  = "), minauc, sep = "")
# maxauct <- paste(c("max(AUC) = "),
#                  maxauc, sep = "")
# legend(
#   0.3,
#   0.6,
#   c(minauct, maxauct, "\n"),
#   border = "white",
#   cex = 1.7,
#   box.col = "white"
# )