## Up-sampling
import numpy as np
from imblearn.over_sampling import SMOTENC
from collections import Counter
features = [
             "ACE_INHIBITORS_DISCHRG","AFIB_FLUTTER_AT_INDEX","AGE_AT_INDEX","ANGIOTENSIN_DISCHRG","ANTI_HTN_PRIOR",
             "API_RIVAROXABAN_DISCHRG","ASPIRIN_DISCHRG","ASPIRIN_PRIOR","ATRIAL_FIB_AT_INDEX","ATRIAL_FLUTTER_AT_INDEX",
             "BETA_BLOCKERS_DISCHRG","BMI_CLOSEST_TO_INDEX","BP_DIASTOLIC","BP_SYSTOLIC","CALCIUM_BLOCKERS_DISCHRG","CHF_AT_INDEX",
             "CHRONIC_KIDNEY_DIS_AT_INDEX","CHRONIC_LIVER_DIS_AT_INDEX","CHRONIC_LUNG_DIS_AT_INDEX","CLOPIDOGREL_DISCHRG",
             "CLOPIDOGREL_PRIOR","COUMAD_WARF_DISCHRG","COUMAD_WARF_PRIOR","CREATININE_CLOSEST_TO_INDEX","DABIGATRAN_DISCHRG",
             "DAYS_BTW_LASTOP_INDEX","DIABETES_AT_INDEX","DIPYRIDAMOLE_DISCHRG","DIPYRIDAMOLE_PRIOR","DIURETICS_DISCHRG",
             "DYSLIPIDEMIA_AT_INDEX","FAM_HEART_HIST","FAM_STROKE_HIST","HB_CLOSEST_TO_INDEX","HBA1C_CLOSEST_TO_INDEX",
             "HDL_CLOSEST_TO_INDEX","HYPERCOAG_STATES_AT_INDEX","HYPERTENSION_AT_INDEX","LDL_CLOSEST_TO_INDEX","MI_AT_INDEX",
             "MILD_LIVER_DIS_AT_INDEX","MOD_SEV_LIVER_DIS_AT_INDEX","NEOPLASM_AT_INDEX","ORAL_ANTICOAG_PRIOR","PERI_VASC_DIS_AT_INDEX",
             "PFO_ALL_TIME","PLT_CLOSEST_TO_INDEX","PT_SEX","RHEUM_DIS_AT_INDEX","SMOKE_STTS","STATINS_DISCHRG","STATINS_PRIOR","WBC_CLOSEST_TO_INDEX"
]
X_train,y_train = trainSet[features], trainSet['RECUR_STROKE']
cat_cols = list(X_train.select_dtypes(include=['object']))
cat_cols_idx = [X_train.columns.get_loc(col) for col in cat_cols]
smote_nc = SMOTENC(categorical_features=cat_cols_idx,k_neighbors=10,sampling_strategy=0.5)
X_train_sm, y_train_sm = smote_nc.fit_resample(X_train,y_train)
trainSet_sm = pd.DataFrame(np.hstack((X_train_sm,y_train_sm[:, None])))
features.append('RECUR_STROKE')
trainSet_sm.columns = features
print(sorted(Counter(y_train).items()))
print(sorted(Counter(y_train_sm).items()))
print(sorted(Counter(trainSet_sm['RECUR_STROKE']).items()))

## Down-sampling
from sklearn.utils import resample
class0 = trainSet[trainSet.RECUR_STROKE==0]
class1 = trainSet[trainSet.RECUR_STROKE==1]
class0_downsample = resample(class0,
                            replace=False,
                            n_samples=661)
trainSet_dwn = pd.concat([class0_downsample,class1])
print(trainSet_dwn.RECUR_STROKE.value_counts())
