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
# About This Script:   - Ensemble method and aggregating predictiongs from 3 models.
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

print("~~ ENSEMBLE STARTED:")

# Global variables - i.e. available to all functions
DATA_FILE <- "../01_ufc_preprocess/UFC_FINAL.csv"

# Define and then load the libraries used in this project
MYLIBRARIES<-c(
  "dplyr",
  "caret",
  "randomForest",
  "sqldf",
  "caTools",
  "neuralnet",
  "e1071"
) 


library(pacman) 
pacman::p_load(char= MYLIBRARIES,install=TRUE,character.only=TRUE) 

# Importing the dataset 
dataset = read.csv(DATA_FILE) 

# Encoding the target feature as factor 
dataset$Winner = factor(dataset$Winner)

#Scaling the dataset
dataset[-1]<-scale(dataset[-1])

#Normalizing the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- cbind(dataset$Winner, as.data.frame(lapply(dataset[-1], normalize)))

set.seed(123) 
split = sample.split(dataset$Winner, SplitRatio = 0.80) 

training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 
#####################################

## NN
nn = readRDS("../08_ufc_web_app/RDS_Files/NN_classifier.rds", refhook = NULL)
nn.results <- neuralnet::compute(nn, test_set)
results_nn <- data.frame(prediction = nn.results$net.result)
predictions_nn<-data.frame(sapply(results_nn,round,digits=0))
predictions_nn$Winner_nn = ifelse(predictions_nn$prediction==1,"Blue","Red")

# RF
rf = readRDS("../08_ufc_web_app/RDS_Files/RF_Classifier.rds", refhook = NULL)
predictions_rf = predict(rf, test_set, type = "vote", norm.votes = TRUE)
predictions_rf = data.frame(predictions_rf)
predictions_rf$Winner_rf = ifelse(predictions_rf$Blue>predictions_rf$Red,"Blue","Red")

# SVM
svm_classifier = readRDS("../08_ufc_web_app/RDS_Files/SVM_RBF_Classifier.rds", refhook = NULL)

y_pred = predict(svm_classifier, newdata = test_set,probability=TRUE)
red_prob = data.frame(attr(y_pred, "probabilities"))$Red
blue_prob = data.frame(attr(y_pred, "probabilities"))$Blue
predictions_svm = data.frame("Blue"=blue_prob,"Red" = red_prob)
predictions_svm$Winner_svm = ifelse(predictions_svm$Blue>predictions_svm$Red,"Blue","Red")


Winner_df = data.frame("NN"=predictions_nn$Winner_nn,"RF"=predictions_rf$Winner_rf,"SVM"=predictions_svm$Winner_svm )
Winner_df$Actual_Winner = test_set$Winner


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


for (row in 1:nrow(Winner_df)) {
  Winner_df$Ensemble_maj_class[row] = Mode(list(as.character(Winner_df$NN[row]),as.character(Winner_df$RF[row]),as.character(Winner_df$SVM[row])))
}
  
for (row in 1:nrow(Winner_df)) {
  Winner_df$Correct_pred = ifelse(as.character(Winner_df$Actual_Winner)==as.character(Winner_df$Ensemble_maj_class),1,0)
}

print(paste("Accuracy of ensemble: ",round(sum(Winner_df$Correct_pred)/nrow(Winner_df),2)))
#No Signisficant Improvement in the predictions

print("~~ ENSEMBLE ENDED:")

