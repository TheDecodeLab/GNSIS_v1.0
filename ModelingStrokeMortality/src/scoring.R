##########
# Scoring
#########
library(pROC)
testSets.imputed<-list()
for(i in 1:3){
  testSets.imputed[[i]]<-pull.data(
    testSets.beforeImp[[i]][c(
      "indicatorMissingA1C",
      "indicatorMissingLDL",
      "indicatorMissingSYSTOLIC",
      "indicatorMissingDIASTOLIC",
      "indicatorMissingLASTOP")],
    testSets.afterImp[[i]]
  )
  testSets.imputed[[i]]<-OneHotEncode(testSets.imputed[[i]])
}
names(testSets.imputed)<-names(testSets.beforeImp)
fetchResults<-function(x,y){
  z<-as.data.frame(x)
  colnames(z)<-names(y)
  return(z)
}
incrementStart<-function(x){4*x-3}
incrementEnd<-function(x){4*x}
results.5fold<-as.data.frame(list())
for (dat in 1:length(testSets.imputed)) {
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions<-setNames(
      data.frame(
        testSets.imputed[[dat]]$label, 
        predict(object = ml.train.5fold[[c]], testSets.imputed[[dat]], type = "prob"),                        # Compute Probability scores
        predict(object = ml.train.5fold[[c]], testSets.imputed[[dat]], type = "raw")                          # Predicted labels
        ),
      c("obs","X0","X1","pred"))
    cm<-confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
      )
    tmp<-as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.5fold[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.5fold[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.5fold[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.5fold<-rbind(results.5fold,
                   tmp%>%
                     mutate(
                       "Classifier" = names(ml.train.5fold[c]),
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
