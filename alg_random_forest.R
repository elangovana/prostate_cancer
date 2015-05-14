predict_timetolive <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  library("randomForest")
  print("----begin function predict_timetolive---")
  #remove other labels, except for the label LKADT_P that is predicted, from the train set  
  subset_train_ct <- train_ct[ , !colnames(train_ct ) %in% c( "DEATH", "DISCONT",  "ENDTRS_C",	"ENTRT_PC")]
  
  print("---subset_train_ct---------")
  print(str(subset_train_ct,list.len = 999 ))
  print("------train_lv------")
  print(str(train_lv,list.len = 999 ))
  print("---test_ct---------")
  print(str(test_ct,list.len = 999 ))
  print("------test_lv------")
  print(str(test_lv,list.len = 999 ))
  
  ## Merge all med information from multiple datasets into one large wide dataset
  # merge train
  subset_train <- merge(subset_train_ct, train_lv, by=0)
  rownames(subset_train) <- subset_train$Row.names
  subset_train <- subset(subset_train, select=-c(Row.names))
  #merge test, remove domain study columns as they are duplicates
  subset_test <- test_ct[, !colnames(test_ct ) %in% c("DOMAIN" , "STUDYID")]
  subset_test <- merge(subset_test, test_lv, by=0)
  rownames(subset_test) <- subset_test$Row.names
  subset_test <- subset(subset_test, select=-c(Row.names))
  
  
  #remove columns with all NA
  subset_train <- subset_train[,colSums(is.na(subset_train))<nrow(subset_train)]
  subset_test <- subset_test[, colSums(is.na(subset_test))<nrow(subset_test)]
 
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  commonCols  <- commonCols[!commonCols %in% c("DOMAIN", "STUDYID")]
  #commonCols <- commonCols[1:6]
  subset_train <- subset_train[, Reduce(union, commonCols, c("LKADT_P"))]
  subset_test <- subset_test[, commonCols]
  
  
  print("------------")
  print(str(subset_train,list.len = 999 ))
  print("------------")
  print(str(subset_test,list.len = 999 ))
  
  #run RF, use rough fix for missing values
  subset_train.roughfix <- na.roughfix(subset_train)
  fit <- randomForest(LKADT_P ~ ., subset_train.roughfix , ntree = 8000) 
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )  
  print(paste ("RMSE:", rmse, sep=" "))
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "timetolive_predictedtraining.csv"))
  
  subset_test.roughfix <- na.roughfix(subset_test)
  predictions = predict(fit, subset_test.roughfix )  
  write.csv( predictions, file=file.path(outdir, "timetolive_predictedtest.csv"))
  
  ##Return model
  print("----end function predict_timetolive---")
  return(predictions)
  
}

predict_death <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                               test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  
  print("----Begin function predict_death---")
  library("randomForest")

  #remove other labels, except for the label LKADT_P that is predicted, from the train set  
   subset_train_ct <- train_ct[ , !colnames(train_ct ) %in% c("LKADT_P" ,  "DISCONT",  "ENDTRS_C",  "ENTRT_PC")]
                              
  ## Merge all med information from multiple datasets into one large wide dataset
  # merge train
  subset_train <- merge(subset_train_ct, train_lv, by=0)
  rownames(subset_train) <- subset_train$Row.names
  subset_train <- subset(subset_train, select=-c(Row.names))
  #merge test, remove domain study columns as they are duplicates
  subset_test <- test_ct[, !colnames(test_ct ) %in% c("DOMAIN" , "STUDYID")]
  subset_test <- merge(subset_test, test_lv, by=0)
  rownames(subset_test) <- subset_test$Row.names
  subset_test <- subset(subset_test, select=-c(Row.names))
  
  
  #remove columns with all NA
  subset_train <- subset_train[,colSums(is.na(subset_train))<nrow(subset_train)]
  subset_test <- subset_test[, colSums(is.na(subset_test))<nrow(subset_test)]
  
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  commonCols  <- commonCols[!commonCols %in% c("DOMAIN", "STUDYID")]
  #commonCols <- commonCols[1:6]
  subset_train <- subset_train[, Reduce(union, commonCols, c("DEATH"))]
  subset_test <- subset_test[, commonCols]
  
  #print data structure for troubleshooting
  print("------------")
  print(str(subset_train,list.len = 999 ))
  print("------------")
  print(str(subset_test,list.len = 999 ))
  
  #Run RF
  subset_train.roughfix <- na.roughfix(subset_train)
  fit <- randomForest(DEATH ~ ., subset_train.roughfix , ntree = 8000)
  percentageCorrect =   (length(subset_train$DEATH[subset_train$DEATH == fit$predicted]) / nrow(subset_train) ) * 100  
  print(paste("Percentage correct:", percentageCorrect, " "))
  
  #Write predicted train data
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "death_predictedtraining.csv"))
  
  #run model on test
  subset_test.roughfix <- na.roughfix(subset_test)
  predictions = predict(fit, subset_test.roughfix )
  write.csv( predictions, file=file.path(outdir, "death_predictedtest.csv"))
  
  
  ##Return model
  print("----End function predict_death---")
  return(predictions)
  
}