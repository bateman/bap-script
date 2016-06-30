# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)


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

csv_file <- ifelse(is.na(args[1]), "input/so_features.csv", args[1])
temp <- read.csv(csv_file, header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                                          time_format="%Y-%m-%d %H:%M:%S")
SO <- temp[[1]]
predictorsNames <- temp[[2]]

choice <- ifelse(is.na(args[2]), "dwolla", args[2])

if(choice == "test") {
  csv_file <- ifelse(is.na(args[3]), "input/test.csv", args[3])
  temp <- read.csv(csv_file, header = TRUE, sep=",")
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                             time_format="%Y-%m-%d %H:%M:%S")
  
  testing <- temp[[1]]
  predictorsNames <- temp[[2]]
} else if(choice == "docusign") { 
  csv_file <- ifelse(is.na(args[3]), "input/docusing.csv", args[3])
  temp <- read.csv(csv_file, header = TRUE, sep=",")
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                              time_format="%d/%m/%Y %H:%M:%S")
  testing <- temp[[1]]
  predictorsNames <- temp[[2]]
} else if(choice == "dwolla") { 
  csv_file <- ifelse(is.na(args[3]), "input/dwolla.csv", args[3])
  temp  <- read.csv(csv_file, header = TRUE, sep=",")
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                            time_format="%d/%m/%y %H:%M")
  testing <- temp[[1]]
  predictorsNames <- temp[[2]]
} else if(choice == "yahoo") { 
  csv_file <- ifelse(is.na(args[3]), "input/yahoo.arff", args[3])
  temp <- read.csv(csv_file, header = TRUE, sep=",")
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%d/%m/%y %H:%M")
  testing <- temp[[1]]
  predictorsNames <- temp[[2]]
} else if(choice == "scn") {
  csv_file <- ifelse(is.na(args[3]), "input/scn.csv", args[3])
  temp <- read.csv(csv_file, header = TRUE, sep=",")
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                         time_format="%Y-%m-%d %H:%M:%S")
  testing <- temp[[1]]
  predictorsNames <- temp[[2]]
} else {
  print("Error: no correct testset name provided.
        Format: Rscript test.R path/to/trainingset.csv testset-name path/to/testset.csv")
  #q(save = "no", status = 1)
}
# remove large unused objects from memory
rm(csv_file)
rm(temp)
# garbage collection
gc()

models_file <- ifelse(is.na(args[4]), "models/models1.txt", args[4])
classifiers <- readLines(models_file)
predictions <- c()

# for model: XYZ
set.seed(875)
# model with optimal parameters.
# modelX <- classifier(solution ~ ., data = training, parameters...)
# modelX.pred <- predict(modelX, testing)
library(caret)
# load all the classifiers to tune

#classifiers <- c("nb")

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

  pred <- predict(model, testing, type = 'prob')
  model.prediction <- prediction(pred[,2], testing$solution)
  predictions <- c(predictions, model.prediction)
}

# finally, save all models predictions to text file ... 

if(!exists("save_predictions", mode="function")) 
  source(paste(getwd(), "lib/save_predictions.R", sep="/"))

save_predictions(outfile = paste(choice, "txt", sep="."), outdir = "output/predictions", 
                 classifiers = classifiers, predictions = predictions)


# and plot ROC and PR curves

line_types <- c(1:length(classifiers))
g_col <- gray.colors(
  length(class),
  start = 0.1,
  end = 0.8,
  gamma = 2.2,
  alpha = NULL
)

if(!exists("plot_curve", mode="function")) 
  source(paste(getwd(), "lib/plot_curve.R", sep="/"))

png(filename="output/plots/roc-curve.png")
plot_curve(predictions=predictions, classifiers=classifiers, 
          colors=g_col, line_type=line_types, 
          x_label="fpr", y_label="tpr")
dev.off()

png(filename="output/plots/pr-curve.png")
plot_curve(predictions=predictions, classifiers=classifiers,
           colors=g_col, line_type=line_types,
           x_label="rec", y_label="prec", leg_pos="bottomleft")
dev.off()

