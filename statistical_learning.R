#############statistical learning#######################################
library(sf)
library(terra)
library(dplyr)
library(future)             # parallel processing
library(lgr)                # logging framework for R
library(mlr3)               # unified interface to machine learning algorithms
library(mlr3learners)       # most important machine learning algorithms
library(mlr3extralearners)  # access to even more learning algorithms
library(mlr3proba)          # here needed for mlr3extralearners::list_learners()
library(mlr3spatiotempcv)   # spatiotemporal resampling strategies
library(mlr3tuning)         # hyperparameter tuning
library(mlr3viz)            # plotting functions for mlr3 objects
library(progressr)          # report progress updates
library(pROC)               # compute roc values