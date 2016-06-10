print("Checking the required packages... will be re-installed if already present.")

# checks if RJava is present
# if not, it has to be installed manually , depending on OS
if(!require("rJava")) {
  print("Please, Install rJava package manually and re-run.")
} else if (!require("RWeka") || !require("RWekajars")) {
  install.packages(c("RWeka", "RWekajars"), dependencies = c("Imports", "Depends"), repos = "http://cran.mirror.garr.it/mirrors/CRAN/")
}

if(!require("xlsx")){
  install.packages(c("xlsx", "xlsxjars"), dependencies = c("Imports", "Depends"), repos = "http://cran.mirror.garr.it/mirrors/CRAN/")
}

# from https://github.com/tobigithub/caret-machine-learning/wiki/caret-ml-setup
# installs most of the 340 caret dependencies but not all of them
mostP <- c("caret", "AppliedPredictiveModeling", "ggplot2", "pROC",
           "data.table", "plyr", "knitr", "shiny", "xts", "lattice", "e1071",
            "klaR", "gpls", "earth", "nnet", "RSNNS", "MASS", "mda", "rpart", "kernlab", 
            "randomForest", "ipred", "gbm", "adabag", "mboost", "caTools", "xgboost", "C50")
install.packages(mostP, dependencies = c("Imports", "Depends"), repos = "http://cran.mirror.garr.it/mirrors/CRAN/")

print("Done.")

# non optimized (unsupported by caret)

# to load in case of ADT:
## WPM("install-package", "alternatingDecisionTrees")
## WPM("load-package", "alternatingDecisionTrees")
## ADT <- make_Weka_classifier("weka/classifiers/trees/ADTree")
## model <- ADT(solution ~ ., data = training)

#SMO:
## see http://www.inside-r.org/packages/cran/RWeka/docs/SMO
## model<-SMO(solution ~ ., data = training, control = Weka_control(K =list("weka.classifiers.functions.supportVector.RBFKernel", G = 2)))


# build a weka classifier from WPM
# make_Weka_classifier <- function(classifier) {
#   model <- NaN
#   require("RWeka")
#   if(classifier == "ADT") {
#     WPM("install-package", "alternatingDecisionTrees")
#     WPM("load-package", "alternatingDecisionTrees")
#     ADT <- make_Weka_classifier("weka/classifiers/trees/ADTree")
#     model <- ADT(solution ~ ., data = training)
#   } else if (classifier == "SMO") {
#     model<-SMO(solution ~ ., 
#                data = training, 
#                control = Weka_control(K =list("weka.classifiers.functions.supportVector.RBFKernel", 
#                                               G = 2))
#     )
#   }
#   
#   return(model)
# }