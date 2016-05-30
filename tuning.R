# comma delimiter
#SO <- read.csv("so_features.csv", header = TRUE)
SO <- read.csv("head.csv", header = TRUE)

# loads caret for param tuning
require(caret)

# create stratified training and test sets from SO dataset
set.seed(825)
inTraining <- createDataPartition(y = SO$solution, 
                                  p = 0.50, 
                                  list = FALSE)
training <- SO[inTraining, ]
testing <- SO[-inTraining, ]

# 100 = 10 x 10 repetitions
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 1,
                           # binary problem
                           summaryFunction=twoClassSummary)


model <- train(solution ~ ., data = training,
               method = "rf",
               trControl = fitControl,
               metric = "ROC",
               tuneLength = 2,
               verbose = FALSE)
print(model)
plot(model)
