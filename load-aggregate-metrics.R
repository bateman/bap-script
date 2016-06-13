# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
fxlsx <- args[1]
fxlsx <- ifelse(is.na(fxlsx), "output/2016-06-06_17.21/aggregate-metrics.xlsx", fxlsx)

library("xlsx")

# load all the classifier tuned
classifiers <- readLines("models.txt")

# default runs
runs <- args[2]
runs <- ifelse(is.na(runs), 2, runs)

# descriptive stats starts after the last run
MIN_INDEX <- runs + 1
MAX_INDEX <- runs + 2
MEAN_INDEX <- runs + 3
MEDIAND_INDEX <- runs + 4
STD_INDEX <- runs + 5

# skips the last one, "parameters"
metrics <- c("AUROC", "F1", "G.mean", "Phi", "Balance", "time")
descrip <- c("min", "max", "mean", "median", "std")

for(i in 1:length(classifiers)){
  nline <- strsplit(classifiers[i], ":")[[1]]
  sheet <- nline[1]
  dat <- read.xlsx(fxlsx, sheetName=sheet)
  dat <- subset(dat, select = AUROC:time)
  metrics_val <- dat[1:runs, ]
  descrip_val <- dat[MIN_INDEX:STD_INDEX, ]

}