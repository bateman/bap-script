# function to save predictions to a file
save_predictions <- function(outfile="predictions.txt", outdir="output/predictions", classifiers, predictions){
  # creates current output directory for current execution
  if(!dir.exists(outdir))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  
    f <- paste(outdir, outfile, sep="/")
    
    for(i in 1:length(predictions)) {
      out <- capture.output(predictions[[i]])
      title=paste("\n===========", classifiers[[i]], "============\n", sep = "  ")
      cat(title, out, file=f, sep="\n", append=ifelse(i==1, FALSE, TRUE))  
    }

}
