library(caret)

scalar_metrics <- function(predictions, truth, outdir="output/scalar", outfile) {
  if(!dir.exists(outdir))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  output_file = paste(outdir, outfile, sep = "/")
  
  CM <- table(data=predictions, reference=truth)
  out <- capture.output(CM)
  cat("\nConfusion Matrix:\n", out, file=output_file, sep="\n", append=FALSE)
  
  TP <- CM[1]
  FP <- CM[3]
  FN <- CM[2]
  TN <- CM[4]
  precision <- posPredValue(predictions, testing[,outcomeName])
  recall <- sensitivity(predictions, testing[,outcomeName])
  TNr <- specificity(predictions, testing[,outcomeName])
  TPr <- recall
  FPr <- FP / (FP + TN)
  
  F1 <- (2 * precision * recall) / (precision + recall)
  out <- paste("F-measure =", F1)
  cat("", out, file=output_file, sep="\n", append=TRUE)
  G <- sqrt(TPr * TNr)
  out <- paste("G-mean =", G)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
  M <- ((TP*TN) - (FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  out <- paste("Matthews phi =", M)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
  B <- 1 - (sqrt((0-FPr)^2 +(1-TPr)^2)/sqrt(2))
  out <- paste("Balance =", B)
  cat("", out, file=output_file, sep = "\n", append = TRUE)
}