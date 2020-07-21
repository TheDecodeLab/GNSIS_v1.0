##:::::::::::::::::::::::::::::
## Built-in Feature Importance
##::::::::::::::::::::::::::::
# Using RandomForest
trainSet.imp$RECUR_STROKE<-as.factor(as.matrix(trainSet.imp %>% 
                                         rename(label="RECUR_STROKE") %>%
                                         mutate(label=factor(label,
                                                             labels=make.names(levels(as.factor(as.character(label)))))) %>% 
                                         select(label)))
builtInRF<-train(RECUR_STROKE~.,
                 data = trainSet.imp %>% 
                          select(-c("PT_ID")),
                 method = "rf",
                 metric = "ROC",
                 trControl = trainControl(classProbs = T,
                                          summaryFunction = twoClassSummary))
varImp(builtInRF)
