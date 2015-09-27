library(caret)
library(doParallel)
library(knitr)

set.seed(1234556) #Give it to me baby, aha, aha! 

directory = "C:/Users/Dan Garcia/Dropbox/Tareas/tarea/MachineLearning/project"
setwd(directory)
training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")

training = training[,!grepl("^X|window|timestamp|user_name", names(training))] #Removing Columns
training = training[, colSums(is.na(training)) < (nrow(training)/2)] #Remove Columns With NAs > 50%

inTrain = createDataPartition(y=training$classe, p=0.70, list=FALSE)

training.set = training[inTrain,]
testing.set = training[-inTrain,]

cl <- makeCluster(3)
registerDoParallel(cl)
inicio = Sys.time()
randomForest.model = train(classe ~ ., 
                            data=training.set, 
                            method="rf", 
                            importance= TRUE,
                            do.trace = TRUE
                          )
fin = Sys.time()
tiempo = fin - inicio
stopCluster(cl)

#randomForest.model = readRDS("randomForest-final-model.rds")
#randomForest.predict.train <- predict(randomForest.model, newdata = training.set)
#randomForest.train.confusionMatrix = confusionMatrix(data=randomForest.predict.train,  training.set$classe)
#randomForest.train.confusionMatrix$overall

#randomForest.train.performance  = data.frame(model = "Training set",
#                                              accuracy = randomForest.train.confusionMatrix$overall[1],
#                                              accuracyLower = randomForest.train.confusionMatrix$overall[3],
#                                              accuracyUpper = randomForest.train.confusionMatrix$overall[4])

#randomForest.predict.test = predict(randomForest.model, newdata = testing.set)
#randomForest.test.confusionMatrix = confusionMatrix(data=randomForest.predict.test,  testing.set$classe)
#randomForest.test.confusionMatrix$overall

#kable(randomForest.test.confusionMatrix$table)