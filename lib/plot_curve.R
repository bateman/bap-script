# Plot multiple ROC curves with different colors and sytles, and with legend and random guess line

library(ROCR)  # for plotting ROC curves

plot_curve <- function(predictions, classifiers, colors, line_types=1, line_widths=1, x_label="fpr", y_label="tpr"){
  par(
    mar = c(5, 5, 2, 2),
    xaxs = "i",
    yaxs = "i",
    cex.axis = 1,
    cex.lab = 1.1
  )
  
  for(i in 1:length(predictions)) {
    adding <- ifelse(i == 1, FALSE, TRUE)
    perf <- performance(predictions[[i]], y_label, x_label)
    plot(perf, add = adding, col = colors[i],  lty = line_types[i], lwd = line_widths)
  }
  
  # Add a legend
  legend(
    "bottomright",
    inset = .1,
    classifiers,
    title = "Classifiers",
    horiz = FALSE,
    lty = c(1:length(classifiers)),
    lwd = 2.5,
    col = colors,
    cex=0.7, pt.cex = 1, box.col = "grey"
  )
  
  # and random guess line
  abline(a = 0, b = 1, lty = "dotted", lwd = 0.3, col="grey")
}