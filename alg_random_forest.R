alg_random_forest <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  library("randomForest")
  
  #convert the train into matrix, labels and featres 
 # subset_train_ct <- train_ct[ , c(1:4, 9:16, 21:130)]
  subset_train_ct <- train_ct[ , c(1:4, 9:16, 21:130)]
  
  print(str(subset_train_ct ))
  
  #remove columns with all NA
  subset_train_ct <- subset_train_ct[,colSums(is.na(subset_train_ct))<nrow(subset_train_ct)]
  
 
 
  print(str(subset_train_ct,list.len = 999 ))
  fit <- randomForest(LKADT_P ~ ., data=subset_train_ct ,  na.action=na.roughfix)
  
   prediction <- predict(fit, test_ct)
   print( data.frame(PassengerId = test_ct$RPT, Survived = Prediction))
  
  
}