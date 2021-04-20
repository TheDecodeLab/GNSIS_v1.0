##################
# Data Imputation
#################
library(caTools)
library(mice)
library(caret)
trainSets.beforeImp<-list()
testSets.beforeImp<-list()
for (dat in 1:3) {
  index<-caTools::sample.split(GNSIS.Case.Control[[dat]]$label,SplitRatio=.8)
  trainSets.beforeImp[[dat]]<-subset(GNSIS.Case.Control[[dat]],index == T)
  testSets.beforeImp[[dat]]<-subset(GNSIS.Case.Control[[dat]],index == F)
  rm(index)
}
names(trainSets.beforeImp)<-names(GNSIS.Case.Control)
for(i in 1:3){
  print(round((table(trainSets.beforeImp[[i]]$label)/length(trainSets.beforeImp[[i]]$label))*100,0))
}
# Case-Control1 design: CASES(7%) CONTROLS(93%)
# Case-Control2 design: CASES(10%) CONTROLS(90%)
# Case-Control3 design: CASES(16%) CONTROLS(84%)
names(testSets.beforeImp)<-names(GNSIS.Case.Control)
for(i in 1:3){
  print(round((table(testSets.beforeImp[[i]]$label)/length(testSets.beforeImp[[i]]$label))*100,0))
}
# Case-Control1 design: CASES(7%) CONTROLS(93%)
# Case-Control2 design: CASES(10%) CONTROLS(90%)
# Case-Control3 design: CASES(16%) CONTROLS(84%)
trainSets.afterImp<-list()
for (dat in 1:3){                                                               # Create S3:mids MICE objects for each train set
  trainSets.afterImp[[dat]]<-mice(trainSets.beforeImp[[dat]][c("label",features)],
                                 m=25,maxit = 25,method = "pmm",seed=99)
}
testSets.afterImp<-list()
for (dat in 1:3){                                                               # Create S3:mids MICE objects for each train set
  testSets.afterImp[[dat]]<-mice(testSets.beforeImp[[dat]][c("label",features)],
                                  m=25,maxit = 25,method = "pmm",seed=99)
}
pull.data<-function(dat,impDat){return(cbind(dat,mice::complete(impDat)))}
OneHotEncode<-function(dat){
  data.encoded<-data.frame(predict(                                             # One hot encode Categorical variables
    dummyVars("~.",data=dat),
    newdata = dat)
  )%>%
    mutate(label=factor(label,                                                  # Rename the label classes to valid R variable names
                        labels = make.names(levels(as.factor(as.character(label))))))
  return(data.encoded)
}
trainSets.imputed<-list()
for(i in 1:3){
  trainSets.imputed[[i]]<-pull.data(
    trainSets.beforeImp[[i]][c(
                          "indicatorMissingA1C",
                          "indicatorMissingLDL",
                          "indicatorMissingSYSTOLIC",
                          "indicatorMissingDIASTOLIC",
                          "indicatorMissingLASTOP")],
    trainSets.afterImp[[i]]
  )
  trainSets.imputed[[i]]<-OneHotEncode(trainSets.imputed[[i]])
}
names(trainSets.imputed)<-names(trainSets.beforeImp)
