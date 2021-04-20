##############
# ML Training
#############
ml.train.baseline<-list()
recordTime.baseline<-list()
ml.train.5fold<-list()
recordTime.5fold<-list()
paramGrid<-trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        summaryFunction = twoClassSummary,                      # Evaluate performance
                        classProbs = T,                                         # Estimate class probabilities
                        allowParallel = T,
                        search = "random")
classifiers<-c("glm","rf","svmRadial","xgbDART")
for (dat in 1:length(trainSets.imputed)) {
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.imputed[dat]),"at",Sys.time()))
    recordTime.5fold[[paste("baseline.started",names(trainSets.imputed[dat]),classifiers[[c]])]]<-Sys.time()
    ml.train.5fold[[paste0(names(trainSets.imputed[dat]),".",classifiers[[c]])]]<-train(label~.,
                                                                                           data = trainSets.imputed[[dat]],
                                                                                           method = classifiers[[c]],
                                                                                           preProcess = c("center","scale"),
                                                                                           metric = "ROC",
                                                                                           trControl = paramGrid,
                                                                                           tuneLength = 5
                                                                                           )
    print(paste("finished:",classifiers[[c]],names(trainSets.imputed[dat]),"at",Sys.time()))
    recordTime.5fold[[paste("baseline.completed",names(trainSets.imputed[dat]),classifiers[[c]])]]<-Sys.time()
  }
}
