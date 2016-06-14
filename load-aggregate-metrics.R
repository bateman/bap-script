# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
fxlsx <- args[1]
fxlsx <- ifelse(is.na(fxlsx), "output/test/aggregate-metrics.xlsx", fxlsx)

library("xlsx")

# load all the classifier tuned
classifiers <- readLines("models.txt")

# default runs
runs <- args[2]
runs <- ifelse(is.na(runs), 10, runs)

# descriptive stats starts after the last run
MIN_INDEX <- runs + 1
MAX_INDEX <- runs + 2
MEAN_INDEX <- runs + 3
MEDIAND_INDEX <- runs + 4
STD_INDEX <- runs + 5

# skips the last one, "parameters"
metrics <- c("AUROC", "F1", "G.mean", "Phi", "Balance", "time")
descrip <- c("min", "max", "mean", "median", "std")

AUROCs = c()
codes <- c()
runs <- c(1,2,3,4,5,6,7,8,9,10)

ccc <- data.frame(x=as.factor(character()), r=numeric(), y=numeric())

for(i in 1:length(classifiers)){
  nline <- strsplit(classifiers[i], ":")[[1]]
  classifier <- nline[1]
  codes <- c(codes, classifier)
  
  dat <- read.xlsx(fxlsx, sheetName=classifier)
  dat <- subset(dat, select = AUROC:time)
  metrics_val <- dat[1:runs, ]
  #descrip_val <- dat[MIN_INDEX:STD_INDEX, ]
  mean_AUROCs <- c(mean_AUROCs, as.numeric(as.character(dat$AUROC))[MEAN_INDEX])
  t <- c(mean_AUROCs, as.numeric(as.character(dat$AUROC)))
}


x.df <- data.frame(x=as.character(codes), y=mean_AUROCs)
library(ScottKnott)

## From: design matrix (dm) and response variable (y)
#sk1 <- SK(x=x.df, y=x.df$y, model='y ~ x', which='x', dispersion='se')
