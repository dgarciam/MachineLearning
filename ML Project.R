library(caret)
library(doParallel)
library(knitr)

set.seed(1234556) #Give it to me baby, aha, aha! 

directory = "C:/Users/Dan Garcia/Dropbox/Tareas/tarea/MachineLearning/project"
setwd(directory)
strings.na <- c("NA","#DIV/0!","")
training = read.csv("pml-training.csv", na.strings = strings.na)
testing = read.csv("pml-testing.csv")

training = training[,!grepl("timestamp|X|user_name|new_window", names(training))]
training = training[, colSums(is.na(training)) == 0] #Remove Columns With NAs > 50%

inTrain = createDataPartition(y=training$classe, p=0.70, list=FALSE)

training.set = training[inTrain,]
testing.set = training[-inTrain,]

cl <- makeCluster(2)
registerDoParallel(cl)
inicio = Sys.time()
randomForest.model = train(classe ~ ., 
                            data=training.set, 
                            method="rf", 
                            importance= TRUE,
                            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 5),
                            do.trace = TRUE
                          )
fin = Sys.time()
tiempo = fin - inicio
stopCluster(cl)

randomForest.model = readRDS("randomForestSaved.rds")
randomForest.predict.train <- predict(randomForest.model, newdata = training.set)
randomForest.train.confusionMatrix = confusionMatrix(data=randomForest.predict.train,  training.set$classe)
randomForest.train.confusionMatrix$overall

randomForest.train.performance  = data.frame(model = "Training set",
                                             accuracy = randomForest.train.confusionMatrix$overall[1],
                                             accuracyLower = randomForest.train.confusionMatrix$overall[3],
                                             accuracyUpper = randomForest.train.confusionMatrix$overall[4])

randomForest.predict.test = predict(randomForest.model, newdata = testing.set)
randomForest.test.confusionMatrix = confusionMatrix(data=randomForest.predict.test,  testing.set$classe)
randomForest.test.confusionMatrix$overall

kable(randomForest.test.confusionMatrix$table)