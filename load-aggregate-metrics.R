# enable commandline arguments from script launched using Rscript
args<-commandArgs(TRUE)
fxlsx <- args[1]

library("xlsx")

# load all the classifier tuned
classifiers <- readLines("models.txt")

for(i in 1:length(classifiers)){
  dat <- read.xlsx(fxlsx, sheetName=classifiers[i])
  print(dat)
}