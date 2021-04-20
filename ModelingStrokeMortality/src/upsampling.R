##############
# Up-Sampling
#############
library(openxlsx)
train.Export<-list()
for(i in 1:3){
  train.Export[[i]]<-pull.data(
    trainSets.beforeImp[[i]][c(
      "indicatorMissingA1C",
      "indicatorMissingLDL",
      "indicatorMissingSYSTOLIC",
      "indicatorMissingDIASTOLIC",
      "indicatorMissingLASTOP")],
    trainSets.afterImp[[i]]
  )
}
names(train.Export)<-names(trainSets.beforeImp)
write.xlsx(train.Export[[1]],"train_30days.xlsx",row.names = F, quote = F)
write.xlsx(train.Export[[2]],"train_90days.xlsx",row.names = F, quote = F)
write.xlsx(train.Export[[3]],"train_365days.xlsx",row.names = F, quote = F)

trainSets.smote.wInd.50per<-list()
trainSets.smote.wInd.50per[[1]]<-OneHotEncode(read_excel("train_30days_sm_wIndicators_50percent.xlsx",na=c("NA"),col_names = T))
trainSets.smote.wInd.50per[[2]]<-OneHotEncode(read_excel("train_90days_sm_wIndicators_50percent.xlsx",na=c("NA"),col_names = T))
trainSets.smote.wInd.50per[[3]]<-OneHotEncode(read_excel("train_365days_sm_wIndicators_50percent.xlsx",na=c("NA"),col_names = T))
names(trainSets.smote.wInd.50per)<-names(trainSets.beforeImp)

trainSets.smote.wInd.100per<-list()
trainSets.smote.wInd.100per[[1]]<-OneHotEncode(read_excel("train_30days_sm_wIndicators_100percent.xlsx",na=c("NA"),col_names = T))
trainSets.smote.wInd.100per[[2]]<-OneHotEncode(read_excel("train_90days_sm_wIndicators_100percent.xlsx",na=c("NA"),col_names = T))
trainSets.smote.wInd.100per[[3]]<-OneHotEncode(read_excel("train_365days_sm_wIndicators_100percent.xlsx",na=c("NA"),col_names = T))
names(trainSets.smote.wInd.100per)<-names(trainSets.beforeImp)
indcatorVars<-c("indicatorMissingA1C",
                "indicatorMissingLDL",
                "indicatorMissingSYSTOLIC",
                "indicatorMissingDIASTOLIC",
                "indicatorMissingLASTOP")
colSums(trainSets.imputed[[1]][indcatorVars])
colSums(trainSets.smote.wInd.50per[[1]][indcatorVars])
colSums(trainSets.smote.wInd.100per[[1]][indcatorVars])
#training
ml.train.5fold.50per.smote<-list()
recordTime.5fold.50per.smote<-list()
for (dat in 1:length(trainSets.smote.wInd.50per)) {
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.smote.wInd.50per[dat]),"at",Sys.time()))
    recordTime.5fold.50per.smote[[paste("baseline.started",names(trainSets.smote.wInd.50per[dat]),classifiers[[c]])]]<-Sys.time()
    ml.train.5fold.50per.smote[[paste0(names(trainSets.smote.wInd.50per[dat]),".",classifiers[[c]])]]<-train(label~.,
                                                                                        data = trainSets.smote.wInd.50per[[dat]],
                                                                                        method = classifiers[[c]],
                                                                                        preProcess = c("center","scale"),
                                                                                        metric = "ROC",
                                                                                        trControl = paramGrid,
                                                                                        tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.smote.wInd.50per[dat]),"at",Sys.time()))
    recordTime.5fold.50per.smote[[paste("baseline.completed",names(trainSets.smote.wInd.50per[dat]),classifiers[[c]])]]<-Sys.time()
  }
}
# scoring
results.5fold.50per.smote<-as.data.frame(list())
for (dat in 1:length(testSets.imputed)) {
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions<-setNames(
      data.frame(
        testSets.imputed[[dat]]$label, 
        predict(object = ml.train.5fold.50per.smote[[c]], testSets.imputed[[dat]], type = "prob"),                        # Compute Probability scores
        predict(object = ml.train.5fold.50per.smote[[c]], testSets.imputed[[dat]], type = "raw")                          # Predicted labels
      ),
      c("obs","X0","X1","pred"))
    cm<-confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp<-as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.5fold.50per.smote[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.5fold.50per.smote[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.5fold.50per.smote[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.5fold.50per.smote<-rbind(results.5fold.50per.smote,
                         tmp%>%
                           mutate(
                             "Classifier" = names(ml.train.5fold.50per.smote[c]),
                             "95%CI"= paste0("(",round(AccuracyLower,3),",",round(AccuracyUpper,3),")")
                           )%>%
                           select(
                             c("Classifier",AUROC="23",AUPR="24","Accuracy","95%CI",NIR="AccuracyNull",
                               "Kappa","Sensitivity","Specificity",
                               "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")
                           )
    )
    rm(tmp,cm,predictions)
  }
}
# training
ml.train.5fold.100per.smote<-list()
recordTime.5fold.100per.smote<-list()
for (dat in 1:length(trainSets.smote.wInd.100per)) {
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.smote.wInd.100per[dat]),"at",Sys.time()))
    recordTime.5fold.100per.smote[[paste("baseline.started",names(trainSets.smote.wInd.100per[dat]),classifiers[[c]])]]<-Sys.time()
    ml.train.5fold.100per.smote[[paste0(names(trainSets.smote.wInd.100per[dat]),".",classifiers[[c]])]]<-train(label~.,
                                                                                                             data = trainSets.smote.wInd.100per[[dat]],
                                                                                                             method = classifiers[[c]],
                                                                                                             preProcess = c("center","scale"),
                                                                                                             metric = "ROC",
                                                                                                             trControl = paramGrid,
                                                                                                             tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.smote.wInd.100per[dat]),"at",Sys.time()))
    recordTime.5fold.100per.smote[[paste("baseline.completed",names(trainSets.smote.wInd.100per[dat]),classifiers[[c]])]]<-Sys.time()
  }
}
# scoring
results.5fold.100per.smote<-as.data.frame(list())
for (dat in 1:length(testSets.imputed)) {
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions<-setNames(
      data.frame(
        testSets.imputed[[dat]]$label, 
        predict(object = ml.train.5fold.100per.smote[[c]], testSets.imputed[[dat]], type = "prob"),                        # Compute Probability scores
        predict(object = ml.train.5fold.100per.smote[[c]], testSets.imputed[[dat]], type = "raw")                          # Predicted labels
      ),
      c("obs","X0","X1","pred"))
    cm<-confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp<-as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.5fold.100per.smote[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.5fold.100per.smote[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.5fold.100per.smote[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.5fold.100per.smote<-rbind(results.5fold.100per.smote,
                         tmp%>%
                           mutate(
                             "Classifier" = names(ml.train.5fold.100per.smote[c]),
                             "95%CI"= paste0("(",round(AccuracyLower,3),",",round(AccuracyUpper,3),")")
                           )%>%
                           select(
                             c("Classifier",AUROC="23",AUPR="24","Accuracy","95%CI",NIR="AccuracyNull",
                               "Kappa","Sensitivity","Specificity",
                               "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")
                           )
    )
    rm(tmp,cm,predictions)
  }
}
