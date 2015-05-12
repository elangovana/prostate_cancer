alg_random_forest <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  library("randomForest")
  
  #remove other labels, except for the label LKADT_P that is predicted, from the train set  
  subset_train_ct <- train_ct[ , c(1:3, 9:16, 21:130)]
  

  ## Merge with other dataset information into one large wide dataset
  # merge train
  subset_train <- merge(subset_train_ct, train_lv, by=0)
  rownames(subset_train) <- subset_train$Row.names
  subset_train <- subset(subset_train, select=-c(Row.names))
  #merge test
  subset_test <- merge(test_ct, test_lv, by=0)
  rownames(subset_test) <- subset_test$Row.names
  subset_test <- subset(subset_test, select=-c(Row.names))
  
  
  #remove columns with all NA
  subset_train <- subset_train[,colSums(is.na(subset_train))<nrow(subset_train)]
  subset_test <- subset_test[, colSums(is.na(subset_test))<nrow(subset_test)]
 
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  subset_train <- subset_train[, commonCols]
  subset_test <- subset_test[, commonCols]
  
 
  #subset_test_ct <- subset(subset_test_ct, select=-c(LKADT_P))
  #print(str(subset_train_ct,list.len = 999 ))
  #print(str(subset_test_ct,list.len = 999 ))
  
  print("------------")
  print(str(subset_train,list.len = 999 ))
  print("------------")
  print(str(subset_test,list.len = 999 ))
  
  fit <- randomForest(LKADT_P ~ ., data=subset_train,  na.action=na.roughfix, ntree = 4000)
  
 
  print(fit$predicted)
  
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )
  
  print("----rmse---")
  print(rmse)
  
  predictions = predict(fit, subset_test)
  
  print(predictions)
  
  ##Return model
  return(fit)
  
}