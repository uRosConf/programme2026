
## Opening the Black Box: Interpreting Machine Learning Models with SHAP Values in R


This short, hands-on tutorial will get you started with SHAP values in R, to explain the predictions of any machine learning model.


[Back to program page](../README.md)


### Basic information

|Item     |entry |
|---------|------|
|Presentor(s) | Thelma Panaïotis ([Pyranhia](https://pyranhia.eu)) |
|Duration |90:00 |
|Materials|[course materials](https://shap-uros2026.pyranhia.eu) (slides, notes, setup), [workshop repository](https://github.com/pyranhia/uros2026-shap-workshop) (code, data, app)|




### Learning Goals

After this course, participants will:

- understand what SHAP values measure, as a fair way to share a prediction between variables
- be able to compute and read SHAP values in R with `DALEX`, for one observation and for a whole dataset
- know when to use TreeSHAP (`shapviz`) and the limits of these explanations


### Course contents

The following topics will be treated:

- from break-down plots to SHAP values: why the order of variables matters
- individual explanations: break-down and SHAP waterfall
- global explanations: importance, beeswarm and dependence plots
- TreeSHAP with `shapviz`
- limits: correlated variables, probability or log-odds scale, no causality


### Target audience

We expect participants to have a working knowledge of R (including the tidyverse) and RStudio projects. Furthermore, some experience with fitting machine learning models is recommendable. No prior knowledge of SHAP values is needed.


### Before coming to the course

Please make sure you have installed R (4.6 or later) and RStudio. The tutorial mainly uses the following packages:

- `DALEXtra` (and `DALEX`)
- `shapviz`
- `tidymodels`, `bonsai` and `lightgbm`

You do not need to install them yourself: clone the [workshop repository](https://github.com/pyranhia/uros2026-shap-workshop) (code, data, model and demo app), then `renv::restore()` installs them at pinned versions, as described on the [setup page](https://shap-uros2026.pyranhia.eu/setup.html). Please do it before the session. Alternatively, you can work on a pre-configured RStudio Server: nothing to install, login details are handed out at the start of the session.

The slides and course notes are on the [course materials](https://shap-uros2026.pyranhia.eu) website.
