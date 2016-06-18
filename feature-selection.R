# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
# files with features
feat_file <- args[1]
feat_file <- ifelse(is.na(feat_file),"features/test.csv", feat_file)
# best k features to select, default 10
k <- args[2]
k <- ifelse(is.na(k), 10, k)

# load the feature file into dataframe dfm 
dfm <- read.csv(feat_file, header = TRUE)
# convert boolean factors 
dfm$has_links<- as.integer(as.logical(dfm$has_links))
# remove answer_uid
dfm <- dfm[ , !(names(dfm) %in% c("answer_uid"))]

# first convert timestamps into POSIX std time values
dfm$date_time <- as.numeric(as.POSIXct(dfm$date_time, format = "'%Y-%m-%d %H:%M:%S'")) # then to equivalent number

# feature selection through Wrapper algorithm
library("Boruta")
# default pValue = 0.01 is used
# try to reduce maxRuns if it takes too long
b.fs <- Boruta(solution~., data=dfm, maxRuns = 100, doTrace = 0, holdHistory = TRUE)

print(b.fs)
plot(b.fs, sort=TRUE)
plotImpHistory(b.fs)


# feature selection through CFS - Correlation Feature Selection
library("FSelector")

# use Pearson’s correlation, requires all data to be continous
weights <- linear.correlation(solution~., data=dfm)
print(weights)
# select K best attributes
subset <- cutoff.k(weights, k)
f <- as.simple.formula(subset, "solution")
print(f)

# use Spearman’s ro correlation, requires all data to be continous
weights <- rank.correlation(solution~., data=dfm)
print(weights)
# select K best attributes
subset <- cutoff.k(weights, k)
f <- as.simple.formula(subset, "solution")
print(f)
