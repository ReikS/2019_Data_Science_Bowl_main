###################################################################################
# PROGRAM NAME :  2019_Data_Science_Bowl_main.R
#
# DESCRIPTION :   This R program serves as a prototype for the kaggle competition
#                 "2019_Data_Science_Bowl".
#
#                 The modeling process is based on the book
#                 "KuhnJohnson_Applied Predictive Modelling_2013"
#                 An online documentation of the caret package can also be found at
#                 http://topepo.github.io/caret/index.html
#                 A list of methods (packages) that can be used via caret can be found at
#                 https://rdrr.io/cran/caret/man/models.html
#
# CONTENT :       1. Initialisation of R session
#
#
# VERSION:        version 0.1
#
# CALLED BY :     none, is the main program
# CALLS TO :      none
#
# PROGRAMMER :    Reik Schottstedt
# DATE WRITTEN :  2020-01-13
####################################################################################
# INPUT FILES :  "sample_submission.csv"
#                "specs.csv"
#                "test.csv"
#                "train.csv"
#                "train_labels.csv"
#
# OUTPUT FILES :
#
####################################################################################
# MODIFICATIONS 0.2 : 
# ---------------------- : #
# DATE : yyyy-mm-dd
# CHANGE 1 : 
# CHANGE 2 : 
# PROGRAMMER : Reik Schottstedt
# DESCRIPTION : #
#################################################################################### 

################################ 1. Initialisation of R session ################################

#### 1.a delete all objects from workspace
rm(list = ls())
memory.limit()

#### 1.b set options
options(scipen=999)

#### 1.c load packages
# define list of required packages
listOfPackages <- list("tidyverse", "coefplot", "caret", "caretEnsemble", 
                       "glmnet", "randomForest", "ranger", "partykit", 
                       "rpart", "rpart.plot", "randomForest", "bst",
                       "LiblineaR", "doParallel")
# load packages
lapply(listOfPackages,require,character.only = TRUE)
rm(listOfPackages)

