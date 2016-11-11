# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)

library(caret)
library(DMwR)
library(RWeka)

set.seed(846)

if(!exists("save_results", mode="function")) 
  source(paste(getwd(), "lib/save_results.R", sep="/"))
if(!exists("scalar_metrics", mode="function")) 
  source(paste(getwd(), "lib/scalar_metrics.R", sep="/"))
if(!exists("setup_dataframe", mode="function")) 
  source(paste(getwd(), "lib/setup_dataframe.R", sep="/"))
# enables multicore parallel processing 
if(!exists("enable_parallel", mode="function")) 
  source(paste(getwd(), "lib/enable_parallel.R", sep="/"))


# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("resolved", "answer_uid", "question_uid", "views", "views_rank", "answers_count",
                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")

# load testing files and predictors
temp <- read.csv("input/docusing.csv", header = TRUE, sep=";")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%d/%m/%Y %H:%M", normalize = FALSE, na_omit = FALSE)
docusign <- temp[[1]]
docusignPredictorsNames <- temp[[2]]
splitIndex <- createDataPartition(docusign[,outcomeName], p = .70, list = FALSE)
docusignTraining <- docusign[splitIndex, ]
docusignTesting <- docusign[-splitIndex, ]
#rm(docusign)

temp <- read.csv("input/dwolla.csv", header = TRUE, sep=";")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%d/%m/%Y %H:%M", normalize = FALSE)
#dwolla <- SMOTE(solution ~ ., data=temp[[1]])
dwolla <- temp[[1]]
dwollaPredictorsNames <- temp[[2]] 
splitIndex <- createDataPartition(dwolla[,outcomeName], p = .70, list = FALSE)
dwollaTraining <- dwolla[splitIndex, ]
#dwollaTraining <- SMOTE(solution ~ ., data=dwolla[splitIndex, ])
dwollaTesting <- dwolla[-splitIndex, ]
#rm(dwolla)

temp <- read.csv("input/yahoo.csv", header = TRUE, sep=";")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
yahoo <- temp[[1]]
yahooPredictorsNames <- temp[[2]]
splitIndex <- createDataPartition(yahoo[,outcomeName], p = .70, list = FALSE)
yahooTraining <- yahoo[splitIndex, ]
yahooTesting <- yahoo[-splitIndex, ]
#rm(yahoo)

temp <- read.csv("input/scn.csv", header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
scn <- temp[[1]]
scnPredictorsNames <- temp[[2]]
splitIndex <- createDataPartition(scn[,outcomeName], p = .70, list = FALSE)
scnTraining <- scn[splitIndex, ]
scnTesting <- scn[-splitIndex, ]
#rm(scn)

# remove large unused objects from memory
rm(temp)
# garbage collection
gc()

models_file <- ifelse(is.na(args[1]), "models/top-models1.txt", args[1])
classifiers <- readLines(models_file)

datasets <- c("dwolla", "docusign", "scn", "yahoo")

# 10-fold CV repetitions
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times, works only with method="repeatedcv", otherwise 1
  repeats = 10,
  #verboseIter = TRUE,
  #savePredictions = TRUE,
  # binary problem
  summaryFunction=twoClassSummary,
  classProbs = TRUE,
  # enable parallel computing if avail
  allowParallel = TRUE,
  returnData = FALSE,
  #sampling = "down",
  preProcOptions = c("center", "scale")
)

for(j in 1:length(datasets)) {
  predictions <- c()
  cmatrices <- c()
  aucs <- c()
  prec_rec <- c()
  
  #dataset_ <- eval(parse(text=datasets[j]))
  cases = nrow(na.omit(dataset_))
  
  training <- paste(datasets[j], "Training", sep = "")
  training <- eval(parse(text=training))
  testing <- paste(datasets[j], "Testing", sep = "")
  testing <- eval(parse(text=testing))
  predictorsNames <- paste(datasets[j], "PredictorsNames", sep = "")
  predictorsNames <- eval(parse(text=predictorsNames))
  
  print(paste("Opened test set", datasets[j]))
  print(paste("Testing performance of zeroR on", datasets[j]))
  
  # no need to train a model, just predict the major (negative) class
  cases <- nrow(na.omit(testing))
  predictions <- rep('False', cases)
  predictions <- as.factor(predictions)
  cm <- confusionMatrix(predictions, testing[,outcomeName], positive = 'True')
  P <- round(cm$byClass['Pos Pred Value'], digits=2)
  R <- round(cm$byClass['Sensitivity'],  digits=2)
  
  # todo load libweka to use the zeroR classifier
    
}
