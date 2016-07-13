# This files generates the ROC plots for the top-ranked (Scott-Knott test) models.
# Here we re-run the classification, with the same seed and param config under which the generated models
# achieved the best performance. Then, we plot the predictions

# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

library(caret)
library(ROCR)
library(pROC)

set.seed(875)

if(!exists("save_results", mode="function")) 
  source(paste(getwd(), "lib/save_results.R", sep="/"))
if(!exists("scalar_metrics", mode="function")) 
  source(paste(getwd(), "lib/scalar_metrics.R", sep="/"))
if(!exists("setup_dataframe", mode="function")) 
  source(paste(getwd(), "lib/setup_dataframe.R", sep="/"))


# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("resolved", "answer_uid", "question_uid", "views", "views_rank",
                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")
#excluded_predictors <- c("resolved", "answer_uid", "question_uid", "upvotes", "upvotes_rank", "views", "views_rank",
#                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")

# load testing files and predictors
temp <- read.csv("input/docusing.csv", header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%d/%m/%Y %H:%M:%S", normalize = FALSE)
docusign <- temp[[1]]
predictorsNames <- temp[[2]]
splitIndex <- createDataPartition(docusign[,outcomeName], p = .70, list = FALSE)
docusignTraining <- docusign[splitIndex, ]
docusignTesting <- docusign[-splitIndex, ]
rm(docusign)

temp <- read.csv("input/dwolla.csv", header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                   time_format="%d/%m/%y %H:%M", normalize = FALSE)
dwolla <- temp[[1]]
predictorsNamesDwolla <- temp[[2]]
splitIndex <- createDataPartition(dwolla[,outcomeName], p = .70, list = FALSE)
dwollaTraining <- dwolla[splitIndex, ]
dwollaTesting <- dwolla[-splitIndex, ]
rm(dwolla)

temp <- read.csv("input/yahoo.csv", header = TRUE, sep=";")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                            time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
yahoo <- temp[[1]]
predictorsNames <- temp[[2]]
splitIndex <- createDataPartition(yahoo[,outcomeName], p = .70, list = FALSE)
yahooTraining <- yahoo[splitIndex, ]
yahooTesting <- yahoo[-splitIndex, ]
rm(yahoo)

temp <- read.csv("input/scn.csv", header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                          time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
scn <- temp[[1]]
predictorsNames <- temp[[2]]
splitIndex <- createDataPartition(scn[,outcomeName], p = .70, list = FALSE)
scnTraining <- scn[splitIndex, ]
scnTesting <- scn[-splitIndex, ]
rm(scn)

# remove large unused objects from memory
rm(temp)
# garbage collection
gc()

models_file <- ifelse(is.na(args[1]), "models/top-models1.txt", args[1])
classifiers <- readLines(models_file)


testsets <- c("dwolla", "docusign", "scn", "yahoo")

# 10-fold CV repetitions
fitControl <- trainControl(
  method = "repeatedcv",
  number = 2,
  ## repeated ten times, works only with method="repeatedcv", otherwise 1
  repeats = 1,
  #verboseIter = TRUE,
  #savePredictions = TRUE,
  # binary problem
  summaryFunction=twoClassSummary,
  classProbs = TRUE,
  # enable parallel computing if avail
  allowParallel = TRUE,
  returnData = FALSE,
  sampling = "smote", 
  preProcOptions = c("center", "scale")
)

for(j in 1:length(testsets)) {
  predictions <- c()
  cmatrices <- c()
  aucs <- c()

  training <- paste(testsets[j], "Training", sep = "")
  training <- eval(parse(text=training))
  testing <- paste(testsets[j], "Testing", sep = "")
  testing <- eval(parse(text=testing))
  
  print(paste("Opened test set", testsets[j]))
  
  # load all the classifiers to test
  for(i in 1:length(classifiers)){
    nline <- strsplit(classifiers[i], ":")[[1]]
    classifier <- nline[1]
    classifiers[i] <- classifier
    
    print(paste("Testing performance of classifier", classifier))
  
    model <- caret::train(solution ~ ., 
                          data = training,
                          method = classifier,
                          trControl = fitControl,
                          metric = "ROC",
                          preProcess = c("center") , #"scale")
                          tuneLength = 1 # values per param
    )
    
    pred_prob <- predict(model, testing[,predictorsNames], type = 'prob')
    model.prediction_prob <- prediction(pred_prob[,2], testing[,outcomeName])
    predictions <- c(predictions, model.prediction_prob)
    aucs <- c(aucs, roc(as.numeric(testing[,outcomeName])-1, pred_prob[,2])$auc)
    aucs <- round(aucs, digits = 2)
    
    pred <- predict(model, testing[,predictorsNames])
    errors <- which(pred != testing[,outcomeName])
    
    # save errors to text file
    save_results(outfile = paste(classifier, "txt", sep="."), outdir = paste("output/misclassifications", testsets[j], sep="/"), 
                 classifiers = c(classifier), results = errors, expanded = TRUE)
    
    cm <- caret::confusionMatrix(table(data=pred, reference=testing[,outcomeName]))
    # save cm to text file
    save_results(outfile = paste(classifier, "txt", sep="."), outdir = paste("output/cm", testsets[j], sep="/"), 
                 classifiers = c(classifier), results = cm, expanded = TRUE)
    scalar_metrics(predictions=pred, truth=testing[,outcomeName], 
                   outdir=paste("output/scalar", testsets[j], sep="/"), outfile=paste(classifier, "txt", sep = "."))
  }

  # finally, save all models predictions to text file ... 
  save_results(outfile = paste(testsets[j], "txt", sep="."), outdir = "output/predictions", 
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
  plot_dir <- paste("output/plots", testsets[j], sep = "/")
  if(!dir.exists(plot_dir))
    dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  
  png(filename=paste(plot_dir, paste(testsets[j], "roc_plot.png", sep="_"), sep = "/"))
  plot_curve(predictions=predictions, classifiers=classifiers, 
             colors=g_col, line_type=line_types, 
             x_label="fpr", y_label="tpr", leg_pos="bottomright", plot_abline=TRUE, 
             leg_title="", main_title="", leg_horiz=FALSE, aucs=aucs)
  dev.off()
  
  png(filename=paste(plot_dir, paste(testsets[j], "pr_plot.png", sep="_"), sep = "/"))
  plot_curve(predictions=predictions, classifiers=classifiers,
             colors=g_col, line_type=line_types,
             x_label="rec", y_label="prec", leg_pos="bottomleft", plot_abline=FALSE,
             leg_title="", main_title="", leg_horiz=FALSE)
  dev.off()
  par(op) #re-set the plot to the default settings
}
