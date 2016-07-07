# enable commandline arguments from script launched using Rscript
args <- commandArgs(TRUE)
fxlsx <- args[1]
fxlsx <- ifelse(is.na(fxlsx), "output/tuning/aggregate-metrics.xlsx", fxlsx)

library("xlsx")

# load all the classifier tuned
classifiers <- readLines("models/models.txt")

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

x <- c()
r <- c()
y <- c()
seq_runs <- seq(runs) #c(1,2,3,4,5,6,7,8,9,10)

dfm <-
  data.frame(x = as.factor(character()),
             r = numeric(),
             y = numeric())

for (i in 1:length(classifiers)) {
  nline <- strsplit(classifiers[i], ":")[[1]]
  classifier <- nline[1]
  r <- c(r, seq_runs)
  
  for(j in 1:runs)
    x <- c(x, classifier)
  
  dat <- read.xlsx(fxlsx, sheetName = classifier)
  dat <- subset(dat, select = AUROC:time)
  metrics_val <- dat[1:runs,]
  descrip_val <- dat[MIN_INDEX:STD_INDEX,]
  AUROCs <- as.numeric(as.character(dat$AUROC[1:runs]))
  y <- c(y, AUROCs)
}

#dfm <- adjust_dataframe(classifiers, x,r,y)
dfm <- data.frame(x, as.numeric(r), y)

library(ScottKnott)
## From: data frame (dfm) and response variable (y)
sk1 <- SK(
  x = dfm,
  y = dfm$y,
  model = 'y ~ x',
  which = 'x',
  dispersion = 's',
  sig.level=.01
)

out <- capture.output(sk1$groups)
cat("*******  Scott-Knott clusters  *******", out, file="output/sk-clusters.txt", sep="\n", append=FALSE)
out <- capture.output(sk1$m.inf)
cat("", out, file="output/tuning/sk-clusters.txt", sep="\n", append=TRUE)

png(filename="output/plots/sk-clusters.png")
plot(sk1,
     #col=grey.colors(n = max(sk1$groups)),
     col=rainbow(max(sk1$groups)),
     mm.lty=3,
     las=2,
     rl=FALSE, xlab="", ylab = "mean AUC", title = "", ylim = c(0,1))
dev.off()

if(!exists("plot_boxplot", mode="function")) 
  source(paste(getwd(), "lib/plot_boxplot.R", sep="/"))

png(filename="output/plots/box-plot.png")
# generate box plot from SK test
plot_boxplot(bx_model=sk1$av$model, x_lab="", y_lab="AUC")
dev.off()
