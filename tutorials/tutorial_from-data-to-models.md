
## From Data to Models: Hands-On Machine Learning with R

This short, hands-on tutorial will get you started with an R tool to implement Machine Learning models as part of the statistical production processes.


[Back to program page](../README.md)


### Basic information

|Item     |entry |
|---------|------|
|Presentor(s) | Adrián Pérez and Sandra Barragán |
|Duration |90:00 |
|Materials|[course materials](./Material_from-data-to-models.zip)|




### Learning Goals


By the end of the tutorial, participants will have a solid understanding of
core supervised learning techniques and the ability to implement them
effectively in R.  

Attendees will work through guided coding exercises using both widely adopted
CRAN packages and custom-developed tools, including the RMLutils
object-oriented R package, which facilitates streamlined workflows with a
common interface for model training, validation, and comparison. Through these
exercises, participants will learn how to preprocess data, fit models, tune
hyperparameters, and assess model performance using appropriate metrics.



### Course contents

The following topics will be treated

- 01 Basic concepts of ML techniques and introduction to RMLutils
- 02 Classification trees and bagging with categorical target
- 03 Boosting algorithms with numerical target


### Target audience

We expect participants to have a working knowledge of base R.
Furthermore, some experience with statistical processes and inferences. 


### Before coming to the course

Please make sure you have installed R and RStudio and have the latest version
of the following package installed:

- [RMLutils](https://github.com/es-ine/rmlutils)
- data.table; MASS; glmnet; ggplot2; rpart; rpart.plot; reshape2; ranger; caret; pROC; gbm; lightgbm.
  
  You can use the following code:
```
install.packages("remotes")
remotes::install_github("es-ine/rmlutils")
#devtools::install_github("es-ine/rmlutils")

if (!require("MASS")) install.packages("MASS")
if (!require("data.table")) install.packages("data.table")
if (!require("glmnet")) install.packages("glmnet")
if (!require("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("h2o", quietly = TRUE)) install.packages("h2o")
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("reshape2")) install.packages("reshape2")
if (!require("ranger")) install.packages("ranger")
if (!require("caret")) install.packages("caret")
if (!require("pROC")) install.packages("pROC")
if (!require("gbm")) install.packages("gbm")
if (!require("lightgbm")) install.packages("lightgbm")
```


