# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

if(!exists("save_results", mode="function")) 
  source(paste(getwd(), "lib/save_results.R", sep="/"))
if(!exists("scalar_metrics", mode="function")) 
  source(paste(getwd(), "lib/scalar_metrics.R", sep="/"))


setup_dataframe <- function(dataframe, outcomeName, excluded_predictors, time_format="%Y-%m-%d %H:%M:%S", 
                            normalize=TRUE, na_omit=TRUE) {
  dataframe <- dataframe[ , !(names(dataframe) %in% excluded_predictors)]
  predictorsNames <- names(dataframe[,!(names(dataframe)  %in% c(outcomeName))]) # removes the var to be predicted from the test set
  
  # convert boolean factors 
  dataframe$has_links<- as.integer(as.logical(dataframe$has_links))
  
  # first check whether thera are leading and trailing apostrophes around the date_time field
  dataframe$date_time <- gsub("'", '', dataframe$date_time)
  # then convert timestamps into POSIX std time values, then to equivalent numbers
  dataframe$date_time <- as.numeric(as.POSIXct(strptime(dataframe$date_time, tz="CET", time_format)))
  
  if(normalize == TRUE) {
    if(!require(e1071))
      library(e1071) # for normality adjustment
    # normality adjustments for indipendent vars (predictors)
    # ln(x+1) transformation mitigates skeweness
    for (i in 1:length(predictorsNames)){
      dataframe[,predictorsNames[i]] <- log1p(dataframe[,predictorsNames[i]])
    }
  }
  
  if(na_omit == TRUE) {
    # exclude rows with Na, NaN and Inf (missing values)
    dataframe <- na.omit(dataframe)
  }
  
  return(list(dataframe, predictorsNames))
}

# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("resolved", "answer_uid", "question_uid", "upvotes", "upvotes_rank", "views", "views_rank",
                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")

csv_file <- ifelse(is.na(args[1]), "input/test.csv", args[1])
temp <- read.csv(csv_file, header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%Y-%m-%d %H:%M:%S")
SO <- temp[[1]]
predictorsNames <- temp[[2]]
rm(temp)
gc()

choice <- ifelse(is.na(args[2]), "test", args[2])
#choice <- "docusign"

if(choice == "docusign") { 
  csv_file <- "input/docusing.csv"
  sep <- ","
  time_format <- "%d/%m/%Y %H:%M:%S"
} else if(choice == "dwolla") { 
  csv_file <- "input/dwolla.csv"
  sep <- ","
  time_format <- "%d/%m/%y %H:%M"
} else if(choice == "yahoo") { 
  csv_file <- "input/yahoo.csv"
  sep <- ";"
  time_format <- "%Y-%m-%d %H:%M:%S"
} else if(choice == "scn") {
  csv_file <- "input/scn.csv"
  sep <- ","
  time_format <- "%Y-%m-%d %H:%M:%S"
} else { ## assume a test is run without param from command line
  csv_file <- "input/head.csv"
  sep <- ","
  time_format <- "%Y-%m-%d %H:%M:%S"
}

# load testing file and predictors
#csv_file <- ifelse(is.na(args[3]), f, args[3])
temp <- read.csv(csv_file, header = TRUE, sep=sep)
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format=time_format)
testing <- temp[[1]]
#predictorsNames <- temp[[2]]

# remove large unused objects from memory
rm(csv_file)
rm(temp)
# garbage collection
gc()

models_file <- ifelse(is.na(args[3]), "models/models1.txt", args[3])
classifiers <- readLines(models_file)
predictions <- c()
cmatrices <- c()

# for model: XYZ
set.seed(875)
# model with optimal parameters.
# modelX <- classifier(solution ~ ., data = training, parameters...)
# modelX.pred <- predict(modelX, testing)
library(caret)
library(ROCR)
# load all the classifiers to tune

for(i in 1:length(classifiers)){
  nline <- strsplit(classifiers[i], ":")[[1]]
  classifier <- nline[1]
  classifiers[i] <- classifier
  
  print(paste("Testing performance of classifier", classifier))
  if(classifier == "nb") {
    grid <- data.frame(fL=0, usekernel=FALSE, adjust=1)
  }
  if(classifier == "rf") {
    grid <- data.frame(mtry=1)
  }
  
  model <- caret::train(solution ~ ., 
                        data = SO,
                        method = classifier,
                        trControl = trainControl(method="none", classProbs = TRUE), #summaryFunction=twoClassSummary, 
                        tuneGrid = grid)
  
  pred_prob <- predict(model, testing[,predictorsNames], type = 'prob')
  model.prediction_prob <- prediction(pred_prob[,2], testing[,outcomeName])
  predictions <- c(predictions, model.prediction_prob)

  pred <- predict(model, testing[,predictorsNames])
  cm <- caret::confusionMatrix(table(data=pred, reference=testing[,outcomeName]))
  # save cm to text file
  save_results(outfile = paste(classifier, "txt", sep="."), outdir = paste("output/cm", choice, sep="/"), 
               classifiers = c(classifier), results = cm, expanded = TRUE)
  scalar_metrics(predictions=pred, truth=testing[,outcomeName], 
                 outdir=paste("output/scalar", choice, sep="/"), outfile=paste(classifier, "txt", sep = "."))
}

# finally, save all models predictions to text file ... 
save_results(outfile = paste(choice, "txt", sep="."), outdir = "output/predictions", 
             classifiers = classifiers, results = predictions, expanded = FALSE)


# and plot ROC and PR curves

line_types <- 1:length(classifiers)
g_col <- gray.colors(
  length(class),
  start = 0.3,
  end = 0.9,
  gamma = 2.2,
  alpha = NULL
)
# g_col <- 1:length(classifiers)

if(!exists("plot_curve", mode="function")) 
  source(paste(getwd(), "lib/plot_curve.R", sep="/"))

op <- par(no.readonly=TRUE) #this is done to save the default settings
plot_dir <- paste("output/plots", choice, sep = "/")
if(!dir.exists(plot_dir))
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

png(filename=paste(plot_dir, "roc-curve.png", sep = "/"))
plot_curve(predictions=predictions, classifiers=classifiers, 
           colors=g_col, line_type=line_types, 
           x_label="fpr", y_label="tpr", leg_pos="bottom", plot_abline=TRUE, 
           leg_title="", main_title=choice)
dev.off()

png(filename=paste(plot_dir, "pr-curve.png", sep = "/"))
plot_curve(predictions=predictions, classifiers=classifiers,
           colors=g_col, line_type=line_types,
           x_label="rec", y_label="prec", leg_pos="bottom", plot_abline=FALSE,
           leg_title="", main_title=choice)
dev.off()
par(op) #re-set the plot to the default settings
