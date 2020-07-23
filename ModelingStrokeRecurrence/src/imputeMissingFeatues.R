library(caret)
library(mice)
set.seed(111)
selected_cols<-c("ACE_INHIBITORS_DISCHRG","AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","ANGIOTENSIN_DISCHRG","ANTI_HTN_PRIOR",
                 "API_RIVAROXABAN_DISCHRG","ASPIRIN_DISCHRG","ASPIRIN_PRIOR","ATRIAL_FIB_AT_INDEX","ATRIAL_FLUTTER_AT_INDEX",
                 "BETA_BLOCKERS_DISCHRG","CALCIUM_BLOCKERS_DISCHRG","CHF_AT_INDEX","CHRONIC_KIDNEY_DIS_AT_INDEX",
                 "CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX","CLOPIDOGREL_DISCHRG","CLOPIDOGREL_PRIOR",
                 "COUMAD_WARF_DISCHRG","COUMAD_WARF_PRIOR","DABIGATRAN_DISCHRG","DIABETES_AT_INDEX","DIPYRIDAMOLE_DISCHRG",
                 "DIPYRIDAMOLE_PRIOR","DIURETICS_DISCHRG","DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","FAM_STROKE_HIST",
                 "HYPERCOAG_STATES_AT_INDEX","HYPERTENSION_AT_INDEX","MI_AT_INDEX","MILD_LIVER_DIS_AT_INDEX",
                 "MOD_SEV_LIVER_DIS_AT_INDEX","NEOPLASM_AT_INDEX","ORAL_ANTICOAG_PRIOR","PERI_VASC_DIS_AT_INDEX","PFO_ALL_TIME",
                 "PT_SEX","RHEUM_DIS_AT_INDEX","SMOKE_STTS","STATINS_DISCHRG","STATINS_PRIOR","HB_CLOSEST_TO_INDEX","PLT_CLOSEST_TO_INDEX",
                 "WBC_CLOSEST_TO_INDEX","BMI_CLOSEST_TO_INDEX","HDL_CLOSEST_TO_INDEX","LDL_CLOSEST_TO_INDEX",
                 "HBA1C_CLOSEST_TO_INDEX","DAYS_BTW_LASTOP_INDEX","CREATININE_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC"
                 ) 
index <- caret::createDataPartition(GNSIS$RECUR_STROKE_CaseControlMatched,p=.8,list=F,times = 1)
trainSet.forImp <- subset(GNSIS,index==TRUE) 
trainSet.afterImp<-mice(trainSet.forImp %>%
                          select(all_of(selected_cols)),
                        m=25,maxit=25,method="cart",ntree=25,seed=99)
testSet.forImp <- subset(GNSIS,index==FALSE) 
testSet.afterImp<-mice(testSet.forImp %>%
                         select(all_of(selected_cols)),
                       m=25,maxit=25,method="cart",ntree=25,seed=99)
pull.data<-function(x,y){
  z<-mice::complete(mice::cbind(x,label=y))
  z<-rename(z,
            "RECUR_STROKE"="label.RECUR_STROKE_CaseControlMatched",
            "PT_ID"="label.PT_ID")
  return(z)
}
trainSet.imp<-pull.data(trainSet.afterImp,trainSet.forImp  %>%
                          select("PT_ID","RECUR_STROKE_CaseControlMatched")
                        )
testSet.imp<-pull.data(testSet.afterImp,testSet.forImp  %>%
                         select("PT_ID","RECUR_STROKE_CaseControlMatched")
                       )