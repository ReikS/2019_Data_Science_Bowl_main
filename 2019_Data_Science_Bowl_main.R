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
# PROGRAMMER :    ReikS
# DATE WRITTEN :  2020-01-13
####################################################################################
# INPUT FILES :  "sample_submission.csv"  rows 1000, cols 2
#                "specs.csv"  rows 386, cols 3
#                "test.csv" rows 28445, cols 11
#                "train.csv"  rows 303319, cols 11
#                "train_labels.csv" rows 17690, cols 7
#
# OUTPUT FILES :
#
####################################################################################
# MODIFICATIONS 0.2 : 
# ---------------------- : #
# DATE : yyyy-mm-dd
# CHANGE 1 : 
# CHANGE 2 : 
# PROGRAMMER : ReikS
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


################################ 2. Set Parameters and directories ################################
par <- list(run_mode        = "production", # possible values: "production", "development"
            on_machine      = "PC", # possible values: "PC", "notebook"
            data_dir        = "E:/Wissensbasis/Projekte/kaggle/2019_Data_Science_Bowl/02_data",
            input_dir       = "/input",
            output_dir      = "/output",
            submission_name = "submission.csv",
            run             = "development_v0.1",
            train_row       = 303319,
            test_row        = 28445,
            seed            = 123456,
            start_date      = as.Date("2008-07-01"),
            end_date        = as.Date("2018-09-30"),
            hold_out        = 0.3,
            k_folds         = 5,
            t_times         = 1,
            pre_processing  = c("medianImpute"),
            target_variable = "accuracy_group",
            predictor = c("pred1", "pred2"))

# set input directory
par$inp <- paste0(par$data_dir, par$input_dir)

#### only on PC set paths, create directories if they don't exists ####
if(par$on_machine == "PC"){
  # output directory
  this_date   <- as.character(Sys.Date())
  date_dir    <- paste0(par$data_dir, par$output_dir, "/", this_date)
  ifelse(!dir.exists(date_dir), dir.create(date_dir), FALSE)
  workdir     <- paste0(date_dir, "/", par$run)
  ifelse(!dir.exists(workdir), dir.create(workdir), FALSE)
  setwd(workdir)
  
  # set output directory to workdir
  par$out <- workdir

  # directory for univariate analysis
  univariate_dir <- paste0(workdir, "/", "univariate")
  ifelse(!dir.exists(univariate_dir), dir.create(univariate_dir), FALSE)
}

if(par$on_machine != "PC"){
  # set output directory
  par$out <- paste0(par$data_dir, par$output_dir)
}

#### parallel computing only possible on PC
if(par$on_machine == "PC"){
  detectCores()
  registerDoParallel(cores=detectCores() - 2)
}

# save parameter set that is used
capture.output(print(par), file = paste0(workdir, "/", "parameter_list_par.csv"))


################ 3. load data from csv ################

#### 3.a "sample_submission.csv"
sample_submission <- read.csv(file = paste0(par$inp, "/", "sample_submission.csv"), stringsAsFactors = FALSE,
                              sep = ",", dec = ".", header = TRUE)
str(sample_submission)
head(sample_submission)
length(unique(sample_submission$installation_id)) == nrow(sample_submission)


#### 3.b "test.csv", "train.csv"
load_train_or_test <- function(par = par, dataset = "test", mode = par$run_mode){
  num_rows <- ifelse(dataset == "test", par$test_row, par$train_row)
  get_rows <- round(ifelse(mode == "development", 0.1, 1.0) * num_rows)
  return(read.csv(file = paste0(par$inp, "/", dataset, ".csv"), stringsAsFactors = FALSE,
    sep = ",", dec = ".", header = TRUE, nrows = get_rows))
}
test <- load_train_or_test(par = par, dataset = "test", mode = par$run_mode)
train <- load_train_or_test(par = par, dataset = "train", mode = par$run_mode)


#### 3.c "specs.csv"
specs <- read.csv(file = paste0(par$inp, "/", "specs.csv"), stringsAsFactors = FALSE,
                  sep = ",", dec = ".", header = TRUE)

#### 3.d "train_labels.csv"
train_labels <- read.csv(file = paste0(par$inp, "/", "train_labels.csv"), stringsAsFactors = FALSE,
                                           sep = ",", dec = ".", header = TRUE)


################ 4. preprocess data ################

#### 4.a train_labels
target <- train_labels %>% dplyr::select(installation_id, accuracy_group) %>%
            dplyr::group_by(installation_id) %>%
            dplyr::mutate(last = dplyr::last(accuracy_group)) %>%
            dplyr::summarise(accuracy_group = max(last))
            
#### 4.b preprocess train and test
# The function 'clean_title'  removes special characters from column title in train/test.
clean_title <- function(x){
  require(dplyr)
  x %>% gsub(pattern = " ", replacement = "") %>%
        gsub(pattern = "-", replacement = "") %>%
        gsub(pattern = "\\(", replacement = "") %>%
        gsub(pattern = "\\)", replacement = "") %>%
        gsub(pattern = "!", replacement = "")
}
# clean_title(test$title)

# The function 'preprocess' 
preprocess <- function(x){
  # 
  x$event_data <- NULL
  x$timestamp <- as.Date(x$timestamp)
  x$title <- clean_title(x$title)
  x$event_code <- paste0("E", as.character(x$event_code))
  
  # create dummy variables
  # x$event_code <- as.factor(x$event_code)
  x$title <- as.factor(x$title)
  x$type <- as.factor(x$type)
  x$world <- as.factor(x$world)
  
  #### function to create dummy variable for factor variables
  # data is a data frame where all columns are factors
  dummy <- function(data, ...) {
    if (length(list(...)[[1]]) > 1) {l1 <- list(...)[[1]]} else {l1 <- list(...)}
    l2 <- lapply(l1, function(n) {
      x <- sapply(unique(na.omit(data[[n]])), function(t)  as.numeric(data[[n]] == t))
      colnames(x) <- paste0(n, sep = "_", unique(na.omit(data[[n]])))
      x
    })
    do.call(cbind,l2)
  }
  
  # get column names of factor variables
  chr_to_factor <- colnames(x)[unname(unlist(lapply(x, is.factor)))]
  
  # those riskfactors that no factor variables (the complementary set)
  no_factor <- setdiff(colnames(x), chr_to_factor)
  
  # create dummies for all factors
  y <- dummy(x, chr_to_factor)
  
  # aggregate dummy variables
  z <- cbind(x %>% dplyr::select(installation_id), as.data.frame(y)) %>%
          dplyr::group_by(installation_id) %>%
          dplyr::summarise_all(list(min = min, max = max, sum = sum, mean = mean, median = median), na.rm = TRUE)
  
  # aggregate the other variables
  a <- x[no_factor] %>% dplyr::select(installation_id, game_time) %>%
          dplyr::group_by(installation_id) %>%
          dplyr::summarise_all(list(min = min, max = max, sum = sum, mean = mean, median = median), na.rm = TRUE)
  
  # combine aggregated data
  return(merge(x = a, y = z, by = "installation_id", all = TRUE))
}

pre_test <- preprocess(test)
pre_train <- merge(x = preprocess(train), y = target, by = "installation_id", all = FALSE)
pre_train <- pre_train[!is.na(pre_train[[par$target_variable]]),]

summary(pre_train)


#### 8. split pre_train in mydata and hold_out, hold_out is kept out of further analysis  ####
set.seed(par$seed)
index <- createDataPartition(pre_train[[par$target_variable]], p = (1.0 - par$hold_out), list = FALSE)
mydata <- pre_train[index,]
hold_out <- pre_train[-index,]
# str(mydata)
# str(hold_out)


####### 9. create indexes for static cross validation folds and save them in myControl  ########
set.seed(par$seed)
folds <- createMultiFolds(mydata[[par$target_variable]], k = par$k_folds, times = par$t_times)

control <- trainControl(
  summaryFunction = defaultSummary,
  classProbs = FALSE,
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = folds
)

####### 10. estimate model ########
#### CART
model_rpart <- caret::train(
  x = mydata %>% dplyr::select(-accuracy_group, -installation_id),
  y = mydata[[par$target_variable]] %>% as.factor() %>% make.names(),
  method = "rpart",
  trControl = control
)

print(model_rpart)
print(model_rpart$finalModel)


#### random forest
model_rf <- caret::train(
 x = mydata %>% dplyr::select(-accuracy_group, -installation_id),
 y = mydata[[par$target_variable]] %>% as.factor() %>% make.names(),
 method = "rf",
 trControl = control,
 preProcess = par$pre_processing
)

print(model_rf)
print(model_rf$finalModel)


####### 11. predict model on training data ########
# The function "class_to_acc_group' converts the predicted classed from caret
# back to numbers.
class_to_acc_group <- function(model_class){
  substr(model_class, 2, 2)
}
# class_to_acc_group(c("X1", "X3"))

submission <- data.frame(installation_id = pre_test$installation_id,
                         accuracy_group  = class_to_acc_group(predict(model_rf, pre_test)))


####### 12. output csv ########
write.csv(x = submission, file = "submission.csv", row.names = FALSE, quote=FALSE)
