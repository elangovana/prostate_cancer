alg_random_forest <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  library("randomForest")
  print("----Begin function alg_random_forest---")
  #remove other labels, except for the label LKADT_P that is predicted, from the train set  
  subset_train_ct <- train_ct[ , c(1:3, 9:16, 21:130)]
  
  
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
  
 
  #subset_test_ct <- subset(subset_test_ct, select=-c(LKADT_P))
  #print(str(subset_train_ct,list.len = 999 ))
  #print(str(subset_test_ct,list.len = 999 ))
  
  print("------------")
  print(str(subset_train,list.len = 999 ))
  print("------------")
  print(str(subset_test,list.len = 999 ))
  
  write.table(subset_test, file ="test.csv", sep=",")
  write.table(subset_train, file ="train.csv", sep=",")
  subset_train.roughfix <- na.roughfix(subset_train)
  print("before RF")
  
  fit <- randomForest(LKADT_P ~ ., subset_train.roughfix , ntree = 8000)
  print("after RF")
 
  print(fit$predicted)
 
  
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )
  
  print("----rmse---")
  print(rmse)
  
  subset_test.roughfix <- na.roughfix(subset_test)
  write.table(subset_test.roughfix, file ="testroughfix.csv", sep=",")
  predictions = predict(fit, subset_test.roughfix )
  
  print(predictions)
  
  ##Return model
  print("----end function alg_random_forest---")
  return(fit)
  
}