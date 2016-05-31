# installs required packages

library("RWeka")
WPM("refresh-cache")
WPM("install-package", "alternatingDecisionTrees")


# to load in case of ADT:
## WPM("load-package", "alternatingDecisionTrees")
## ADT <- make_Weka_classifier("weka/classifiers/trees/ADTree")

#SMO:
## see http://www.inside-r.org/packages/cran/RWeka/docs/SMO