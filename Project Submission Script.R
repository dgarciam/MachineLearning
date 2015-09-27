library(caret)
library(doParallel)
library(knitr)
library(dplyr)
library(rpart)
library(kernlab)
library(reshape2)

set.seed(1234556) #Give it to me baby, aha, aha! 

directory = "C:/Users/Dan Garcia/Dropbox/Tareas/tarea/MachineLearning/project"
setwd(directory)
testing = read.csv("pml-testing.csv")

testing = testing[,!grepl("^X|window|timestamp|user_name", names(testing))] #Removing Columns
testing = testing[, colSums(is.na(testing)) < (nrow(testing)/2)] #Remove Columns With NAs > 50%

randomForest.model = readRDS("randomForest-final-model.rds")
randomForest.test = predict(randomForest.model, newdata = testing)

# Coursera Submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

answers = randomForest.test
pml_write_files(answers)

