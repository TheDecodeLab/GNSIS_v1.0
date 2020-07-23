##:::::::::::::::::::::::::::::
## Built-in Feature Importance
##:::::::::::::::::::::::::::::
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
# Using LASSO method 
x<-trainSet.imp %>% 
    select(-c("PT_ID","RECUR_STROKE","PT_SEX","SMOKE_STTS"))
y<-trainSet.imp["RECUR_STROKE"]
builtInLassoCaret<-train(x,y,
                         preProcess ="scale",
                         metric="ROC",
                         method="glmnet",
                         trControl = trainControl(method="cv", 
                                                  number=10,
                                                  classProbs = T,
                                                  summaryFunction = twoClassSummary),
                         tuneGrid = expand.grid(alpha = 1,
                                                lambda = seq(0.0001,0.1,by = 0.001)))
builtInLassoCaret$bestTune
coef(builtInLassoCaret$finalModel, builtInLassoCaret$bestTune$lambda)
##:::::::::::::::
## Filter Methods
##:::::::::::::::
# Study Correlation
colnames(trainSet.imp[apply(trainSet.imp,2,function(x){all(x %in% 0:1)})==FALSE])
corr<-cor(trainSet.imp %>%
            select("AGE_AT_INDEX",
                   "BP_SYSTOLIC",
                   "BP_DIASTOLIC",
                   "PLT_CLOSEST_TO_INDEX",
                   "WBC_CLOSEST_TO_INDEX",
                   "BMI_CLOSEST_TO_INDEX",
                   "HDL_CLOSEST_TO_INDEX",
                   "LDL_CLOSEST_TO_INDEX",
                   "HBA1C_CLOSEST_TO_INDEX",
                   "HB_CLOSEST_TO_INDEX",
                   "DAYS_BTW_LASTOP_INDEX",
                   "CREATININE_CLOSEST_TO_INDEX"),
          method = "pearson")
# Univariate Filters
sbfWithFilter<-sbf(RECUR_STROKE~.,
                   data=trainSet.imp %>% 
                          select(-c("PT_ID")),
                   sbfControl=sbfControl(functions=caretSBF,method="repeatedcv",repeats=10))
sbfWithFilter["optVariables"]
##:::::::::::::::::
## Wrapper Methods
##::::::::::::::::
# Boruta
library(Boruta)
library(dplyr)
boruta_output <- Boruta(RECUR_STROKE~.,
                        data=trainSet.imp %>% 
                          select(-c("PT_ID")),
                        doTrace=1)
getSelectedAttributes(boruta_output,withTentative = TRUE)
TentativeRoughFix(boruta_output)
imps <- attStats(TentativeRoughFix(boruta_output))
imps2 <- imps[imps$decision !='Rejected',c('meanImp','decision')]
imps2[order(-imps2$meanImp),]
# Recursive feature elimination
caretFuncs$summary <- twoClassSummary
control<-rfeControl(functions=caretFuncs,
                    method = "repeatedcv",
                    repeats = 10,
                    verbose = F) 
rfeProfile<-rfe(x,y,
                rfeControl = control,
                method = "svmRadial",
                metric = "ROC",
                trControl = trainControl(classProbs = T,
                                         summaryFunction = twoClassSummary))
rfeProfile
rfeProfile$fit
predictors(rfeProfile)
