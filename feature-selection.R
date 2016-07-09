# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
# files with features
feat_file <- args[1]
feat_file <- ifelse(is.na(feat_file),"input/esej_features_85k.csv", feat_file)
# best k features to select, default 10
k <- args[2]
k <- ifelse(is.na(k), 10, k)

# name of outcome var to be predicted
outcomeName <- "solution"
# list of predictor vars by name
excluded_predictors <- c("resolved", "answer_uid", "question_uid",
                         "has_code_snippet", "has_tags", "loglikelihood_descending_rank", "F.K_descending_rank")

if(!exists("setup_dataframe", mode="function")) 
  source(paste(getwd(), "lib/setup_dataframe.R", sep="/"))
# load the feature file into dataframe dfm 
temp <- read.csv(feat_file, header = TRUE, sep=",")
temp <- setup_dataframe(dataframe = temp, outcomeName = outcomeName, excluded_predictors = excluded_predictors,
                        time_format="%Y-%m-%d %H:%M:%S", normalize = FALSE)
dfm <- temp[[1]]
predictorsNames <- temp[[2]]

library(DMwR)
dfm <- SMOTE(solution ~ ., data=dfm, perc.under = 100, perc.over = 700)

# output file for the classifier at hand
output_dir <- "output/feature-selection"
if(!dir.exists(output_dir))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
output_file <- paste(output_dir, "feature-subset.txt", sep = "/")

# feature selection through Wrapper algorithm
library("Boruta")
# default pValue = 0.01 is used
# try to reduce maxRuns if it takes too long
b.fs <- Boruta(solution~., data=dfm, maxRuns = 100, doTrace = 2, holdHistory = TRUE)
out <- capture.output(b.fs)
cat("*******  BORUTA  *******", out, file=output_file, sep="\n", append=FALSE)
out <- capture.output(attStats(b.fs))
cat("", out, file=output_file, sep="\n", append=TRUE)
#plot(b.fs, sort=TRUE)
#plotImpHistory(b.fs)


# feature selection through CFS - Correlation Feature Selection
library("FSelector")

# use Pearson's correlation, requires all data to be continous
weights <- linear.correlation(solution~., data=dfm)
out <- capture.output(weights)
cat("\n*******  CFS Pearson's correlation  *******", out, file=output_file, sep="\n", append=TRUE)
# select K best attributes
subset <- cutoff.k(weights, k)
out <- capture.output(subset)
cat("", out, file=output_file, sep="\n", append=TRUE)
#f <- as.simple.formula(subset, "solution")
#print(f)
# use Spearman's ro correlation, requires all data to be continous
weights <- rank.correlation(solution~., data=dfm)
out <- capture.output(weights)
cat("\n*******  CFS Spearman’s ro correlation  *******", out, file=output_file, sep="\n", append=TRUE)
# select K best attributes
subset <- cutoff.k(weights, k)
out <- capture.output(subset)
cat("", out, file=output_file, sep="\n", append=TRUE)
#f <- as.simple.formula(subset, "solution")
#print(f)