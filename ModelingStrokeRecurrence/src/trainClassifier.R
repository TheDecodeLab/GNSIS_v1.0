##:::::::::::::::::::::
## Classifier training
##:::::::::::::::::::::
library(caret)
label<-c("RECUR_STROKE")
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
