# caret
# https://github.com/tobigithub/caret-machine-learning/wiki/caret-ml-setup

# installs required packages
if (!require("pacman")) 
  install.packages("pacman", dependencies = TRUE)

pacman::p_load(caret, klaR, gpls, earth, nnet, RSNNS, MASS, 
               mda, rpart, kernlab, randomForest, ipred, gbm, 
               fastAdaboost, mboost, caTools, xgboost, C50)

# checks if RJava is present
# if not, it has to be installed manually
if(!require("RJava")) {
  print("Please, Install RJava package manually and re-run.")
} else if (!require("RWeka") || !require("RWekajars")) {
  pacman::p_load(RWeka, RWekajars)
}

# non optimized (unsupported by caret)

# to load in case of ADT:
# WPM("refresh-cache")
# WPM("install-package", "alternatingDecisionTrees")
## WPM("load-package", "alternatingDecisionTrees")
## ADT <- make_Weka_classifier("weka/classifiers/trees/ADTree")
## model <- ADT(solution ~ ., data = training)

#SMO:
## see http://www.inside-r.org/packages/cran/RWeka/docs/SMO
## model<-SMO(solution ~ ., data = training, control = Weka_control(K =list("weka.classifiers.functions.supportVector.RBFKernel", G = 2)))

