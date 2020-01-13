###################################################################################
# PROGRAM NAME :  2019_Data_Science_Bowl_main.R
#
# DESCRIPTION :   The programs loads one table that contains the target variable for
#                 the model development and all potential predictors. In case of a 
#                 risk model the target variable is an (observed) risk parameter
#                 (default criterion, EAD, LGD, SEQ, B-LGD,...) and the potential
#                 predictors are the available risk drivers.
#
#                 In case of the BLGD-champion-challenger analysis the relevant
#                 table is d008p.blgd19_int_without_cap from the gDWH. 
#
#                 At first the R session is initialised and parameters for the
#                 specific analysis are set (parts 1, 2). Then, several data 
#                 transformations are done, which are specific for the model analysis
#                 so that the data can be used in carets train function (parts 3, 4, 5, 7). 
#                 In part 6 a univariate analysis of the target variable and predictores is done.
#                 Afterwards the cross validation is prepared such that the same folds are 
#                 used for each model. Then, the program estimates several models to explain
#                 the target variable using multivariate predictive models with 
#                 (potential) risk drivers from the same table.
#                 Finally, the performance of the different models is estimated and 
#                 compared using different plots.
#
#                 The modeling process is based on the book
#                 "KuhnJohnson_Applied Predictive Modelling_2013"
#                 An online documentation of the caret package can also be found at
#                 http://topepo.github.io/caret/index.html
#                 A list of methods (packages) that can be used via caret can be found at
#                 https://rdrr.io/cran/caret/man/models.html
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
# INPUT FILES :  
#
#
#
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

