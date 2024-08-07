directoryextractor.func <- function(selected.file){
  filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
  filename <- filename.vector[length(filename.vector)]
  setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
}


#I need to add a pulling in of alt datasets to see how this can go
data <- file.choose()
directoryextractor.func(data)
data <- read.table(data)

data.ref <- data[data$V2 == 1,]
data.ref$class <- "pass"
data.nref <- data[data$V2 == 2,]
data.nref$class <- "fail"

#in theory this is all the non-ref data cleaned to a similar shape?
data.test <- data[data$V2==99,-c(2:4)]

  
data.allref <- rbind(data.ref, data.nref)
data.allref <- data.allref[,-c(2:4),]



library(caret)
library(mlbench)

set.seed(998)
inTraining <- createDataPartition(data.allref$class, p = .85, list = FALSE)
training <- data.allref[ inTraining,]
testing  <- data.allref[-inTraining,]
#testing  <- data.test

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 50,
  ## repeated ten times
  repeats = 50)

start.time <- Sys.time()
gbmFit1 <- train(class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
end.time <- Sys.time()
total.time.1 <- end.time-start.time; total.time.1
gbmFit1

predictions <- predict(gbmFit1, testing)
testing$class <- as.factor(testing$class)
gbmFit1.preds <- confusionMatrix(predictions, testing$class); gbmFit1.preds

gbmGrid <-  expand.grid(interaction.depth = c(10, 15, 20, 25, 30, 35, 40, 45, 48, 50), 
                        #n.trees = (1:30)*5,
                        n.trees = (1:50)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = 25)

nrow(gbmGrid)

set.seed(825)
start.time <- Sys.time()
gbmFit2 <- train(class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
end.time <- Sys.time()
total.time.2 <- end.time - start.time; total.time.2
gbmFit2
predictions <- predict(gbmFit2, testing)
predictions.data <- predict(gbmFit2, data.test) #this is testing the rull data, i should def add in the pass/fail results to verify. maybe run the whole ref pop as training?
data.test$assignment <- predictions.data
gbmFit2.preds <- confusionMatrix(predictions, testing$class); gbmFit2.preds


trellis.par.set(caretTheme())
plot(gbmFit2)  
trellis.par.set(caretTheme())
plot(gbmFit2, metric = "Kappa")

write.csv(data.test[,c(1,98)], "new.data.test.csv")

