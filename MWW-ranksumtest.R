# Mann-Whitney-Wilcoxon rank sum test
# check if any difference exist between cross-platform and within-platform
# predictions models.
# Also, effect size (delta) is computed to measure that difference.

require(effsize)


cross_gbm <- c(0.80, 0.83, 1.89, 1.04, 1.45)
whithin_gbm <- c(1.15, 0.88, 0.90, 0.74, 1.21)
U = wilcox.test(cross_gbm, whithin_gbm, alternative = "two.sided", 
                paired = TRUE, correct = FALSE, conf.level = 0.95)
print(U$p.value)

effsize = cliff.delta(cross_gbm, whithin_gbm, return.dm=FALSE)
print(effsize)


cross_xgbtree <- c(0.80, 0.83, 1.89, 1.04, 1.45)
whithin_xgbtree <- c(1.15, 0.88, 0.90, 0.74, 1.21)
U = wilcox.test(cross_xgbtree, whithin_xgbtree, alternative = "two.sided", paired = TRUE, correct = FALSE)
print(U$p.value)

effsize = cliff.delta(cross_xgbtree, whithin_xgbtree, return.dm=FALSE)
print(effsize)
