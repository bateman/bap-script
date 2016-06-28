# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

library(caret) # for param tuning
library(e1071) # for normality adjustment

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

# for model: XYZ

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

preds <- c(pred, pred2, pred3)

class <- c('gpls', 'rf', 'svmlinear')
line_types <- c(1:length(class))
g_col <- gray.colors(
  length(class),
  start = 0.1,
  end = 0.8,
  gamma = 2.2,
  alpha = NULL
)

if(!exists("plot_curve", mode="function")) 
  source(paste(getwd(), "plot_curve.R", sep="/"))
png(filename="output/plots/roc-curve.png")
plot_curve(predictions=preds, classifiers=class, 
           colors=g_col, line_type=line_types, 
           x_label="fpr", y_label="tpr")
dev.off()

perf <- performance(pred,"prec","rec")
perf2 <- performance(pred2, "prec", "rec")
perf3 <- performance(pred3, "prec", "rec")

png(filename="output/plots/pr-curve.png")
plot_curve(predictions=preds, classifiers=class, 
           colors=g_col, line_type=line_types, 
           x_label="rec", y_label="prec")
dev.off()

