######################
# Data Pre-processing
######################
library(readxl)
library(dplyr)
features<-c(
  "HBA1C_MEDIAN_BEFORE_INDEX",
  "LDL_MEDIAN_BEFORE_INDEX",
  "BP_SYSTOLIC",
  "BP_DIASTOLIC",
  "DAYS_BTW_LASTOP_INDEX",
  "WBC_MEDIAN_BEFORE_INDEX",
  "PLT_MEDIAN_BEFORE_INDEX",
  "HB_MEDIAN_BEFORE_INDEX",
  "INDEX_INSURANCE_TYPE",
  "CREATININE_CLOSEST_TO_INDEX",
  "PT_SEX",
  "SMOKE_STTS",
  "FAM_HEART_HIST",
  "FAM_STROKE_HIST",
  "AFIB_FLUTTER_PREINDEX",
  "HYPERTENSION_PREINDEX",
  "MI_PREINDEX",
  "DIABETES_PREINDEX",
  "DYSLIPIDEMIA_PREINDEX",
  "CHF_PREINDEX",
  "HYPERCOAG_STATES_PREINDEX",
  "CHRONIC_LIVER_DIS_PREINDEX",
  "CHRONIC_LUNG_DIS_PREINDEX",
  "RHEUM_DIS_PREINDEX",
  "CHRONIC_KIDNEY_DIS_PREINDEX",
  "NEOPLASM_PREINDEX",
  "PERI_VASC_DIS_PREINDEX",
  "PFO_ALL_TIME",
  "AGE_AT_INDEX",
  "BMI_MEDIAN_BEFORE_INDEX",
  "MIGRAINE_PREINDEX",
  "CONVULSIONS_PREINDEX",
  "EPILEPSY_PREINDEX",
  "DEPRESSION_PREINDEX",
  "MANIC_BIPOLAR_DIS_PREINDEX",
  "ANXIETY_DIS_PREINDEX",
  "CONVERSION_DIS_PREINDEX",
  "SYNCOPE_PREINDEX",
  "ALCOHOL_DEP_ABUSE_PREINDEX",
  "MULTIPLE_SCLEROSIS_PREINDEX",
  "ESRD_PREINDEX",
  "PERIPHERAL_NEUROPATHY_PREINDEX",
  "BRAIN_TUMOR_PREINDEX",
  "HEPATIC_ENCEPHALOPATHY_PREINDEX",
  "CIRRHOSIS_PREINDEX",
  "MENIERE_VERTIGO_PREINDEX"
)
GNSIS<-left_join(
  read_excel("GNSIS_CASE_CONTROL_30_90_365.xlsx",na=c("NA"),col_names = T),
  read_excel("GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx",na=c("NA"),col_names=T),
  by=c("PT_ID"))%>%
  filter(AGE_AT_INDEX>18)%>%
  mutate(indicatorMissingA1C=if_else(is.na(HBA1C_MEDIAN_BEFORE_INDEX),1,0),     # Create a binary variable to indicate patient with missing A1c value
         indicatorMissingLDL=if_else(is.na(LDL_MEDIAN_BEFORE_INDEX),1,0),
         indicatorMissingSYSTOLIC=if_else(is.na(BP_SYSTOLIC),1,0),
         indicatorMissingDIASTOLIC=if_else(is.na(BP_DIASTOLIC),1,0),
         indicatorMissingLASTOP=if_else(is.na(DAYS_BTW_LASTOP_INDEX),1,0),
         PT_SEX=if_else(PT_SEX=="Female",1,0),                                  # Re-code Patient Sex to a binary variable
         SMOKE_STTS=if_else(SMOKE_STTS=="CURRENT SMOKER",1,0),                  # Dissolve Smoking Status to only 2 levels
         INDEX_INSURANCE_TYPE=if_else(INDEX_INSURANCE_TYPE %in% 
                                        c("Self Pay","Special Billing","VA",NA),# Combine fewer% insurance categories into OTHERS
                                          "Others",INDEX_INSURANCE_TYPE)  
         )
GNSIS.Case.Control<-list()
case.control.def<-function(var,control){
  GNSIS%>%
    mutate(label = if_else(!!enquo(var) == control,0,1))%>%                     # Dynamically set feature as parameter  
    select("PT_ID","label",indicatorMissingA1C,indicatorMissingLDL,indicatorMissingSYSTOLIC,indicatorMissingDIASTOLIC,indicatorMissingLASTOP,features)
}
GNSIS.Case.Control[["30Days"]]<-case.control.def(CASE_CONTROL1,"CONTROL1")
GNSIS.Case.Control[["90Days"]]<-case.control.def(CASE_CONTROL2,"CONTROL2")
GNSIS.Case.Control[["365Days"]]<-case.control.def(CASE_CONTROL3,"CONTROL3")
missingness<-round((colSums(is.na(GNSIS.Case.Control[[2]]))/length(unique(GNSIS.Case.Control[[2]]$PT_ID)))*100,2)
missingness<-round((colSums(is.na(subset(GNSIS.Case.Control[[2]],label==1)))/length(unique(subset(GNSIS.Case.Control[[2]],label==1)$PT_ID)))*100,2)
View(missingness)
