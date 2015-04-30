alg_random_forest <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  library("randomForest")
  
  #convert the train into matrix, labels and featres 
 # subset_train_ct <- train_ct[ , c(1:4, 9:16, 21:130)]
  subset_train_ct <- train_ct[ , c(1:3, 9:16, 21:130)]
 

  
  #remove columns with all NA
  subset_train_ct <- subset_train_ct[,colSums(is.na(subset_train_ct))<nrow(subset_train_ct)]
  subset_test_ct <- test_ct[, colnames(subset_train_ct)]
 
  subset_test_ct <- subset_test_ct[, colSums(is.na(subset_test_ct))<nrow(subset_test_ct)]
  subset_train_ct <- subset_train_ct[, colnames(subset_test_ct)]

 subset_test_ct <- subset(subset_test_ct, select=-c(LKADT_P))
  #print(str(subset_train_ct,list.len = 999 ))
  #print(str(subset_test_ct,list.len = 999 ))
  
 
  fit <- randomForest(LKADT_P ~ ., data=subset_train_ct ,  na.action=na.roughfix, ntree = 4000)
 
  print(subset_train_ct$LKADT_P)
  print(fit$predicted)
 
  rmse = sqrt( sum( (subset_train_ct$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train_ct) )
 
  print(rmse)
 
  predictions = predict(fit, subset_test_ct)
 
  print(predictions)

  ##Return model
  return(fit)
  
}