# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

if(!exists("save_results", mode="function")) 
  source(paste(getwd(), "lib/save_results.R", sep="/"))
if(!exists("scalar_metrics", mode="function")) 
  source(paste(getwd(), "lib/scalar_metrics.R", sep="/"))
if(!exists("setup_dataframe", mode="function")) 
  source(paste(getwd(), "lib/setup_dataframe.R", sep="/"))


# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("resolved", "answer_uid", "question_uid",
                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")
#excluded_predictors <- c("resolved", "answer_uid", "question_uid", "upvotes", "upvotes_rank", "views", "views_rank",
#                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")

csv_file <- ifelse(is.na(args[1]), "input/esej_features_171k.csv", args[1])
temp <- read.csv(csv_file, header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
SO <- temp[[1]]
predictorsNames <- temp[[2]]

choice <- ifelse(is.na(args[2]), "so", args[2])
#choice <- "docusign"

if(choice == "so") {
  seeds <- readLines("seeds.txt")
  set.seed(seeds[length(seeds)])
  splitIndex <- createDataPartition(SO[,outcomeName], p = .70, list = FALSE)
  testing <- SO[-splitIndex, ]
  library(DMwR)
  SO <- SMOTE(solution ~ ., data=SO[splitIndex, ], perc.under = 100, perc.over = 700)
  #summary(SO$solution)
} else {
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
  temp <- read.csv(csv_file, header = TRUE, sep=sep)
  temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                          time_format=time_format)
  testing <- temp[[1]]
}

# remove large unused objects from memory
rm(csv_file)
rm(temp)
# garbage collection
gc()

models_file <- ifelse(is.na(args[3]), "models/top-models.txt", args[3])
classifiers <- readLines(models_file)
predictions <- c()
cmatrices <- c()
aucs <- c()

# for model: XYZ
set.seed(875)
# model with optimal parameters.
# modelX <- classifier(solution ~ ., data = training, parameters...)
# modelX.pred <- predict(modelX, testing)
library(caret)
library(ROCR)
library(pROC)
# load all the classifiers to tune

for(i in 1:length(classifiers)){
  nline <- strsplit(classifiers[i], ":")[[1]]
  classifier <- nline[1]
  classifiers[i] <- classifier
  
  print(paste("Testing performance of classifier", classifier))
  if(classifier == "xgbTree") {
    grid <- data.frame(nrounds = 200, 
                       max_depth = 4, 
                       eta = 0.1, 
                       gamma = 0, 
                       colsample_bytree = 1, 
                       min_child_weight = 1)
  }
  else if(classifier == "gbm") {
    grid <- data.frame(n.trees = 250, 
                       interaction.depth = 3, 
                       shrinkage = 0.1,
                       n.minobsinnode = 10)
  }
  else if(classifier == "pcaNNet") {
    grid <- data.frame(size = 7, decay = 0.1)
  }
  else if(classifier == "earth") {
    grid <- data.frame(nprune = 15, degree = 1)
  }
  
  model <- caret::train(solution ~ ., 
                        data = SO,
                        method = classifier,
                        trControl = trainControl(method="none", classProbs = TRUE), #sampling = "smote"),  
                        tuneGrid = grid,  preProcess = c("center", "scale"))
  
  pred_prob <- predict(model, testing[,predictorsNames], type = 'prob')
  model.prediction_prob <- prediction(pred_prob[,2], testing[,outcomeName])
  predictions <- c(predictions, model.prediction_prob)
  aucs <- c(aucs, roc(as.numeric(testing[,outcomeName])-1, pred_prob[,2])$auc)
  aucs <- round(aucs, digits = 2)
  
  pred <- predict(model, testing[,predictorsNames])
  errors <- which(pred != testing[,outcomeName])

  # save errors to text file
  save_results(outfile = paste(classifier, "txt", sep="."), outdir = paste("output/misclassifications", choice, sep="/"), 
               classifiers = c(classifier), results = errors, expanded = TRUE)
  
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
# g_col <- gray.colors(
#   n = length(classifiers),
#   start = 0.3,
#   end = 0.8,
#   gamma = 2.2,
#   alpha = NULL
# )
g_col <- rainbow(length(classifiers))

if(!exists("plot_curve", mode="function")) 
  source(paste(getwd(), "lib/plot_curve.R", sep="/"))

op <- par(no.readonly=TRUE) #this is done to save the default settings
plot_dir <- paste("output/plots", choice, sep = "/")
if(!dir.exists(plot_dir))
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

png(filename=paste(plot_dir, paste(choice, "roc_plot.png", sep="_"), sep = "/"))
plot_curve(predictions=predictions, classifiers=classifiers, 
           colors=g_col, line_type=line_types, 
           x_label="fpr", y_label="tpr", leg_pos="bottomright", plot_abline=TRUE, 
           leg_title="", main_title="", leg_horiz=FALSE, aucs=aucs)
dev.off()

png(filename=paste(plot_dir, paste(choice, "pr_plot.png", sep="_"), sep = "/"))
plot_curve(predictions=predictions, classifiers=classifiers,
           colors=g_col, line_type=line_types,
           x_label="rec", y_label="prec", leg_pos="bottomleft", plot_abline=FALSE,
           leg_title="", main_title="", leg_horiz=FALSE)
dev.off()
par(op) #re-set the plot to the default settings
