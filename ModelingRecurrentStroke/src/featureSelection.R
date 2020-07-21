##:::::::::::::::::::::::::::::
## Built-in Feature Importance
##::::::::::::::::::::::::::::
# Using RandomForest
y<-as.factor(as.matrix(trainSet.imp %>% 
                         rename(label="RECUR_STROKE") %>%
                         mutate(label=factor(label,
                                             labels=make.names(levels(as.factor(as.character(label)))))) %>%
                         select(label)))
x<-trainSet.imp
x$RECUR_STROKE<-as.factor(as.matrix(trainSet.imp %>% 
                                         rename(label="RECUR_STROKE") %>%
                                         mutate(label=factor(label,
                                                             labels=make.names(levels(as.factor(as.character(label)))))) %>% 
                                         select(label)))
builtInRF<-train(RECUR_STROKE~.,
                 data=x %>% 
                   select(-c("PT_ID")),
                 method="rf",
                 metric="ROC",
                 trControl = trainControl(classProbs = T,
                                          summaryFunction = twoClassSummary))
varImp(builtInRF)
