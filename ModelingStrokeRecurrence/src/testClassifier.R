test<-list()
confMatrix<-list()
results<-as.data.frame(list())
auROC <- NULL
incrementEnd<-function(x){6*x}
incrementStart<-function(x){6*x-5}
fetchResults<-function(x,y){
  z<-as.data.frame(x)
  colnames(z)<-names(y)
  return(z)
}
test <-list(
  Encode(testSet.imp,features1),
  Encode(testSet.imp,features2),
  Encode(testSet.imp,features3),
  Encode(testSet.imp,features4)
)
names(test)<-c("fullSet","expertSel","dataDriven","combo")
for(i in 1:length(test)){
  for(c in incrementStart(i):incrementEnd(i)){
    confMatrix[[paste(names(ml.trained[c]),".",names(test[i]),sep="")]]<-confusionMatrix(
      reference = test[[i]]$RECUR_STROKE,
      data = predict(object = ml.trained[[c]],
                     test[[i]] %>%
                       select(-c(label)),
                     type = "raw"),
      mode = 'everything',
      positive = 'X1')
    auROC<-rbind(auROC,
                 data.frame("Classifier" = paste(names(ml.trained[c]),".",names(test[i]),sep=""),
                            "AUROC"=roc(predictor=predict(object = ml.trained[[c]],
                                                          test[[i]] %>% 
                                                            select(-c(label)),
                                                          type = "prob")$X1,
                                        response = test[[i]][,'RECUR_STROKE'],
                                        levels=rev(levels(test[[i]][,'RECUR_STROKE'])))$auc*100))
  }
}
for(i in 1:length(confMatrix)){
  results<-rbind(results,
                 as.data.frame(t(rbind(fetchResults(confMatrix[[i]]$byClass,confMatrix[i]),
                                       fetchResults(confMatrix[[i]]$overall,confMatrix[i]),
                                       fetchResults(as.data.frame(confMatrix[[i]]$table)$Freq,confMatrix[i])
                 )))
  )
}
results<-cbind(auROC,results %>%  
                 mutate(Accuracy=Accuracy*100,NIR=AccuracyNull*100,Sensitivity=Sensitivity*100,
                        Specificity=Specificity*100,
                        Precision=Precision*100,Prevalence=Prevalence*100,
                        "95%CI"=paste0("(",round(AccuracyLower*100,2),",",round(AccuracyUpper*100,2),")")
                 ) %>%
                 select(c("Accuracy","95%CI","NIR","Kappa","Sensitivity","Specificity",
                          "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")))
