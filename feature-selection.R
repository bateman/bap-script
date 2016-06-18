# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
# files with features
feat_file <- args[1]
feat_file <- ifelse(is.na(feat_file),"features/test.csv", feat_file)
# best k features to select, default 10
k <- args[2]
k <- ifelse(is.na(k), 10, k)

# load the feature file into dataframe dfm 
dfm <- read.csv(feat_file)

# feature selection through Wrapper algorithm
library("Boruta")
# default pValue = 0.01 is used
# try to reduce maxRuns if it takes too long
b.fs <- Boruta(Solution~., data=dfm, maxRuns = 100, doTrace = 0, holdHistory = FALSE)

print(b.fs)
plot(b.fs, sort=TRUE)
plotImpHistory(b.fs)


# feature selection through CFS - Correlation Feature Selection
library("FSelector")

# use Pearson’s correlation
weights <- linear.correlation(Solution~., data=dfm)
print(weights)
# select K best attributes
subset <- cutoff.k(weights, k)
f <- as.simple.formula(subset, "Solution")
print(f)

# use Spearman’s ro correlation
weights <- rank.correlation(Solution~., data=dfm)
print(weights)
# select K best attributes
subset <- cutoff.k(weights, k)
f <- as.simple.formula(subset, "Solution")
print(f)
