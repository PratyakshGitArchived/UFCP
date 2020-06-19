# ************************************************
# No License.
# The model is for educational purposes only
# ************************************************
# 2019 PRATICAL BUSINESS ANALYTICS - University of Surrrey
#
# Group Name   : The Deepest Learners
# November-December 2019
# ************************************************
# Data Analysis for UFC fight data 1993-2019
# About This Script:   - Builds and Evaluates a Single-Layer Neural Network Classifier
#                      - note: Initially 0.8 train/test ratio was used and got 
#                              ~68% accuracy,
#                              but for representation purposes a ratio of 0.1
#                              is substituted to train faster. Therefore, a 
#                              lower accuracy may be shown.
#                      - note: Contains NN Factor Importance chart in Plots
#
# ************************************************

# Set the currect source file as Working Directory
sourceFile<- dirname(parent.frame(2)$ofile)
setwd(sourceFile)

gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

#  clears all objects in "global environment"
rm(list=ls())

print("~~ NN STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "../01_ufc_preprocess/UFC_FINAL.csv"

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "nnet",
  "e1071",
  "caret"
) 

library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# ************************************************
# NreadDataset() : (Tweaked)
# Read a CSV file from working directory
#
# INPUT: string  - csvFilename - CSV filename
#        Boolean - optional stringAsFactors (default TRUE)
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset <- function(csvFilename, stringAsFactors=TRUE){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = stringAsFactors)
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# prepare_data()
# Prepares a given data set for models by generating training/testing set (includes randomisation & normalisation)
#
# INPUT:    data frame         - data set to be handled
#           integer            - output_col_n, index of the output 
#           double             - ratio, train to test size ratio
#
# OUTPUT :  list (of 4)        - toReturn, returns the traing & testing set along with target and testing categories
# ************************************************
prepare_data <- function(df, output_col_n, ratio){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  
  # Extract training set
  df_train <- df_normalised[random,] 
  # Extract testing set
  df_test <- df_normalised[-random,]
  
  # Output category
  df_target_category <- df[random,output_col_n]
  df_test_category <- df[-random,output_col_n]
  
  toReturn <- list("training_set" = df_train, "testing_set" = df_test,"target_category" = df_target_category,"test_category"= df_test_category)
  
  return(toReturn)
}


# ************************************************
# main code goes below:
# ************************************************

UFC_DATA <- NreadDataset(DATA_FILE) # Normal dataset
print("Running ... ")

# Train/test split with 0.8 ratio   # for representation changed to 0.1
prepared_dataset <- prepare_data(df = UFC_DATA, output_col_n = 1, ratio = 0.1)

# k-fold cross validation (k=3)
fitControl <- trainControl(method = "repeatedcv",
                           number = 3, 
                           repeats = 3)

# NN classifier
nn_classifier <- train(prepared_dataset$training_set, prepared_dataset$target_category,
                 method = "pcaNNet", # performs with PCA
                 trControl = fitControl,
                 verbose = FALSE,
                 trace = T)

nnetplot3 = plot(varImp(nn_classifier, scale = F),main="Neural Network: Factor Importance")
print(nnetplot3)

# NN Predictions on unseen testing set
nnpred <- predict(nn_classifier, prepared_dataset$testing_set)

# Confusion Matrix
print("NN Consufion Matrix:")
cm <- confusionMatrix(nnpred, prepared_dataset$test_category)
print(cm)

# Print accuracy
print(paste("NN Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))


print("~~ NN ENDED:")


