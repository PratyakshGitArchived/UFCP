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
# About This Script:     - Builds and Evaluates a Logistics Regression Classifier
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

print("~~ Logistics Regression STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "../01_ufc_preprocess/UFC_FINAL.CSV"
PCA_FILE <- "../01_ufc_preprocess/UFC_PCA.csv"


# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "caTools",
  "dplyr",
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
# prepare_data()    (tweaked)
# Prepares a given data set for models by generating training/testing set (includes randomisation & normalisation)
#
# INPUT:    data frame         - data set to be handled
#           integer            - output_col_n, index of the output 
#           double             - ratio, train to test size ratio
#           Boolean            - optional normalized (default TRUE), performs normalisation
#
# OUTPUT :  list (of 4)        - toReturn, returns the traing & testing set along with target and testing categories
# ************************************************
prepare_data <- function(df, output_col_n, ratio, normalized = TRUE){
  # Randomisation with train to test ratio
  random <- sample(1:nrow(df), ratio * nrow(df))
  
  # Normalisation
  normalised <- function(x) {return((x - min(x,rm.na=TRUE))/(max(x,rm.na=TRUE)-min(x,rm.na=TRUE)))}
  if(normalized == TRUE){
    df_normalised <- as.data.frame(lapply(df[,-output_col_n], normalised))
  }
  else{
    df_normalised <- df[,-output_col_n]
  }
  
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
UFC_PCA <- NreadDataset(PCA_FILE) # PCA data
PCA_cols <- names(UFC_PCA)[2:length(names(UFC_PCA))] # PCA column names excluding Winner
print("Running ... ")

UFC_DATA$Winner = as.character(UFC_DATA$Winner) # Factor to chr
UFC_DATA$Winner[UFC_DATA$Winner == "Blue"] <- 1 # encode blue as 1
UFC_DATA$Winner[UFC_DATA$Winner == 'Red']  <- 0 # encode red as 0
UFC_DATA$Winner = as.numeric(UFC_DATA$Winner) # as numeric

output_col_n = 1 # output target

# Train/test split with 0.7 ratio with no normalisation 
prepared_dataset <- prepare_data(UFC_DATA, output_col_n = output_col_n,ratio = 0.7, normalized = FALSE)

# PCA train/test are subset of prepared_dataset with PCA_cols only
pca_train <- prepared_dataset$training_set %>% select(PCA_cols)
pca_test <- prepared_dataset$testing_set %>% select(PCA_cols)

# Concat X and y for NN training
pca_glm_data <- data.frame(Winner=prepared_dataset$target_category, pca_train)
glm_classifier <- glm(Winner ~. , data = pca_glm_data , family = 'binomial')

# GLM Predictions on unseen testing set
glmpred <- predict(glm_classifier, prepared_dataset$testing_set, type="response")
glmpred[glmpred > 0.5] <- 1 # if probability > 0.5, classify as 1 (Blue) otherwise 0 (Red)
glmpred[glmpred <= 0.5] <- 0
glmpred <- as.factor(glmpred) # convert to factor

# Confusion Matrix
print("PCA Logistics Regression Consufion Matrix:")
cm <- confusionMatrix(glmpred, as.factor(prepared_dataset$test_category))
print(cm)

# Print accuracy
print(paste("PCA_GLM Accuracy in",length(prepared_dataset$test_category),"unseen data:", round(cm$overall[1], 4)*100,"%" ))

print("~~ Logistics Regression ENDED:")









