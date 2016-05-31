# comma delimiter
#SO <- read.csv("so_features.csv", header = TRUE)
SO <- read.csv("head.csv", header = TRUE)

# converts boolean factors 
#SO$solution <- as.factor(SO$solution)
SO$has_links<- as.logical.factor(SO$has_links)

# converts timestamps into POSIX std time values
SO$date_time <- as.POSIXct(SO$date_time, tz = "GMT", format = "'%Y-%m-%d %H:%M:%S'")

# normality distrib check for indipendent vars (predictors)
library(e1071) # load e1071                     
# ln(x+1) transformation for mitigating skeweness
SO$answers_count <- log1p(SO$answers_count)
SO$answers_count <- log1p(SO$time_difference)
SO$answers_count <- log1p(SO$time_difference_rank)
SO$answers_count <- log1p(SO$len)
SO$answers_count <- log1p(SO$len_rank)
SO$answers_count <- log1p(SO$wordcount)
SO$answers_count <- log1p(SO$wordcount_rank)
SO$answers_count <- log1p(SO$avg_chars_per_word)
SO$answers_count <- log1p(SO$avg_chars_per_word_rank)
SO$answers_count <- log1p(SO$sentences)
SO$answers_count <- log1p(SO$sentences_rank)
SO$answers_count <- log1p(SO$avg_words_per_sentence)
SO$answers_count <- log1p(SO$avg_words_per_sentence_rank)
SO$answers_count <- log1p(SO$longest_sentence)
SO$answers_count <- log1p(SO$longest_sentence_rank)
SO$answers_count <- log1p(SO$loglikelihood)
SO$answers_count <- log1p(SO$loglikelihood_ascending_rank)
SO$answers_count <- log1p(SO$F.K)
SO$answers_count <- log1p(SO$F.K_ascending_rank)
SO$answers_count <- log1p(SO$has_links)

# loads caret for param tuning
library(caret)

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
                           repeats = 10,
                           # binary problem
                           summaryFunction=twoClassSummary,
                           classProbs = TRUE,
                           # enable parallel computing if avail
                           allowParallel = TRUE)


model <- train(solution ~ ., data = training,
               method = "rf",
               trControl = fitControl,
               metric = "ROC",
               tuneLength = 5, # five values per param
               verbose = FALSE)
model

#p <- predict(model, testing, type="prob")
#summary(p)
#plot(p)
