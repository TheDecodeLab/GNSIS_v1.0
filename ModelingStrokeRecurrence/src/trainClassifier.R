##:::::::::::::::::::::
## Classifier training
##:::::::::::::::::::::
library(caret)
label<-c("RECUR_STROKE")
# fullset
features1<-c("ACE_INHIBITORS_DISCHRG","AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","ANGIOTENSIN_DISCHRG","ANTI_HTN_PRIOR",
             "API_RIVAROXABAN_DISCHRG","ASPIRIN_DISCHRG","ASPIRIN_PRIOR","ATRIAL_FIB_AT_INDEX","ATRIAL_FLUTTER_AT_INDEX",
             "BETA_BLOCKERS_DISCHRG","BMI_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC","CALCIUM_BLOCKERS_DISCHRG","CHF_AT_INDEX",
             "CHRONIC_KIDNEY_DIS_AT_INDEX","CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX","CLOPIDOGREL_DISCHRG",
             "CLOPIDOGREL_PRIOR","COUMAD_WARF_DISCHRG","COUMAD_WARF_PRIOR","CREATININE_CLOSEST_TO_INDEX","DABIGATRAN_DISCHRG",
             "DAYS_BTW_LASTOP_INDEX","DIABETES_AT_INDEX","DIPYRIDAMOLE_DISCHRG","DIPYRIDAMOLE_PRIOR","DIURETICS_DISCHRG",
             "DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","FAM_STROKE_HIST","HB_CLOSEST_TO_INDEX","HBA1C_CLOSEST_TO_INDEX",
             "HDL_CLOSEST_TO_INDEX","HYPERCOAG_STATES_AT_INDEX","HYPERTENSION_AT_INDEX","LDL_CLOSEST_TO_INDEX","MI_AT_INDEX",
             "MILD_LIVER_DIS_AT_INDEX","MOD_SEV_LIVER_DIS_AT_INDEX","NEOPLASM_AT_INDEX","ORAL_ANTICOAG_PRIOR",
             "PERI_VASC_DIS_AT_INDEX","PFO_ALL_TIME","PLT_CLOSEST_TO_INDEX","PT_SEX","RHEUM_DIS_AT_INDEX",
             "SMOKE_STTS","STATINS_DISCHRG","STATINS_PRIOR","WBC_CLOSEST_TO_INDEX")
# expect selection
features2<-c("AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","BMI_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC","CHF_AT_INDEX",
             "CHRONIC_KIDNEY_DIS_AT_INDEX","CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX","CREATININE_CLOSEST_TO_INDEX",
             "DIABETES_AT_INDEX","DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","FAM_STROKE_HIST","HB_CLOSEST_TO_INDEX",
             "HBA1C_CLOSEST_TO_INDEX","HDL_CLOSEST_TO_INDEX","HYPERCOAG_STATES_AT_INDEX","NEOPLASM_AT_INDEX",
             "HYPERTENSION_AT_INDEX","LDL_CLOSEST_TO_INDEX","MI_AT_INDEX",
             "PERI_VASC_DIS_AT_INDEX","PFO_ALL_TIME","PLT_CLOSEST_TO_INDEX","PT_SEX","RHEUM_DIS_AT_INDEX",
             "SMOKE_STTS","WBC_CLOSEST_TO_INDEX")
# data driven
features3<-c("ACE_INHIBITORS_DISCHRG","AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","ANGIOTENSIN_DISCHRG","ANTI_HTN_PRIOR",
             "API_RIVAROXABAN_DISCHRG","ASPIRIN_PRIOR","ATRIAL_FIB_AT_INDEX","ATRIAL_FLUTTER_AT_INDEX","BETA_BLOCKERS_DISCHRG",
             "BMI_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC","CALCIUM_BLOCKERS_DISCHRG","CHF_AT_INDEX","CHRONIC_KIDNEY_DIS_AT_INDEX",
             "CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX","CLOPIDOGREL_PRIOR","COUMAD_WARF_DISCHRG","COUMAD_WARF_PRIOR",
             "CREATININE_CLOSEST_TO_INDEX","DAYS_BTW_LASTOP_INDEX","DIABETES_AT_INDEX","DIPYRIDAMOLE_PRIOR","DIURETICS_DISCHRG",
             "DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","HB_CLOSEST_TO_INDEX","HBA1C_CLOSEST_TO_INDEX","HDL_CLOSEST_TO_INDEX",
             "HYPERTENSION_AT_INDEX","LDL_CLOSEST_TO_INDEX","MI_AT_INDEX","MILD_LIVER_DIS_AT_INDEX","ORAL_ANTICOAG_PRIOR",
             "PERI_VASC_DIS_AT_INDEX","PFO_ALL_TIME","PLT_CLOSEST_TO_INDEX","PT_SEX","RHEUM_DIS_AT_INDEX","SMOKE_STTS",
             "STATINS_DISCHRG","STATINS_PRIOR","WBC_CLOSEST_TO_INDEX")
# combo
features4<-c("AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","BMI_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC",
             "CHF_AT_INDEX","CHRONIC_KIDNEY_DIS_AT_INDEX","CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX",
             "CREATININE_CLOSEST_TO_INDEX","DIABETES_AT_INDEX","DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","HB_CLOSEST_TO_INDEX",
             "HBA1C_CLOSEST_TO_INDEX","HDL_CLOSEST_TO_INDEX","HYPERTENSION_AT_INDEX","LDL_CLOSEST_TO_INDEX","MI_AT_INDEX",
             "PERI_VASC_DIS_AT_INDEX","PFO_ALL_TIME","PLT_CLOSEST_TO_INDEX","PT_SEX","RHEUM_DIS_AT_INDEX","SMOKE_STTS",
             "WBC_CLOSEST_TO_INDEX")
Encode <- function(data,features){
  data.encoded<-cbind(data$RECUR_STROKE,
                      data.frame(predict(dummyVars(RECUR_STROKE~.,
                                                   data = data %>% 
                                                     select(all_of(features),label),levelsOnly = TRUE),
                                         newdata = data %>% 
                                           select(all_of(features),label))))
  data.encoded <- dplyr::rename(data.encoded,'RECUR_STROKE'='data$RECUR_STROKE')
  data.encoded <- data.encoded %>% 
    mutate(RECUR_STROKE=factor(RECUR_STROKE,
                               labels=make.names(levels(as.factor(as.character(RECUR_STROKE))))))
  return(data.encoded)
}
train<-list()
ml.trained<-list()
recordRunTime<-list()
train <-list(
  Encode(trainSet.imp,features1),
  Encode(trainSet.imp,features2),
  Encode(trainSet.imp,features3),
  Encode(trainSet.imp,features4)
)  
names(train)<-c("fullSet","expertSel","dataDriven","combo")
classifiers <- list("glm","gbm","rf","xgbDART","svmRadial","C5.0")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          summaryFunction = twoClassSummary,
                          classProbs = T,
                          allowParallel = T,
                          search = "random")
for(i in 1:length(train)){
  for(c in 1:length(classifiers)){
    recordRunTime[[paste(names(train[i]),".",classifiers[[c]],sep="")]]<-Sys.time()
    ml.trained[[paste(names(train[i]),".",classifiers[[c]],sep="")]] <-train(RECUR_STROKE~.,
                                                                             data=train[[i]],
                                                                             method=classifiers[[c]],
                                                                             preProcess = "scale",
                                                                             metric = "ROC",
                                                                             trControl = paramGrid,
                                                                             tuneLength = 10)
  }
}
