library(ROCR)

# see http://stackoverflow.com/questions/18130338/plotting-an-roc-curve-in-glmnet
# https://mlr-org.github.io/mlr-tutorial/release/html/roc_analysis/index.html
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
pred2 <- prediction(abs(ROCR.simple$predictions +
                          rnorm(length(
                            ROCR.simple$predictions
                          ), 0, 0.1)),
                    ROCR.simple$labels)
pred3 <- prediction(abs(ROCR.simple$predictions +
                          rnorm(length(
                            ROCR.simple$predictions
                          ), 0, 0.11)),
                    ROCR.simple$labels)
perf <- performance(pred, "tpr", "fpr")
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")
par(
  mar = c(5, 5, 2, 2),
  xaxs = "i",
  #?
  yaxs = "i",
  #?
  cex.axis = 1.1,
  cex.lab = 1.2
)
g <- gray.colors(
  3,
  start = 0.2,
  end = 0.7,
  gamma = 2.2,
  alpha = NULL
)
plot(perf,
     col = g,
     lty = 1,
     lwd = 1)
plot(perf2,
     add = TRUE,
     col = g,
     lty = 2,
     lwd = 1.3
)
plot(perf3,
     add = TRUE,
     col = g,
     lty = 3,
     lwd = 1.3
)

# Add a legend
legend(
  "bottomright",
  inset = .15,
  c("1", "2", "3"),
  title = "xx",
  horiz = FALSE,
  lty = c(1:3),
  lwd = 2,
  col = g
)

abline(a = 0,
       b = 1,
       lty = "dotted",
       lwd = 0.3)
#
# # calculating the values for ROC curve
# pred <- prediction(target_pred, target_class)
# perf <- performance(pred, "tpr", "fpr")
# # changing params for the ROC plot - width, etc
# par(
#   mar = c(5, 5, 2, 2),
#   xaxs = "i",
#   yaxs = "i",
#   cex.axis = 1.3,
#   cex.lab = 1.4
# )
# plotting the ROC curve
# plot(perf,
#      col = "black",
#      lty = 3,
#      lwd = 3)
# # calculating AUC
# auc <- performance(pred, "auc")
# # now converting S4 class to vector
# auc <- unlist(slot(auc, "y.values"))
# # adding min and max ROC AUC to the center of the plot
# minauc <- min(round(auc, digits = 2))
# maxauc <- max(round(auc, digits = 2))
# minauct <- paste(c("min(AUC)  = "), minauc, sep = "")
# maxauct <- paste(c("max(AUC) = "),
#                  maxauc, sep = "")
# legend(
#   0.3,
#   0.6,
#   c(minauct, maxauct, "\n"),
#   border = "white",
#   cex = 1.7,
#   box.col = "white"
# )