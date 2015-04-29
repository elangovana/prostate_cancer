alg_random_forest <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  library("randomForest")
  
  #convert the train into matrix, labels and featres 
  m_train_ct <- as.matrix(train_ct[ 1:nrow(train_ct), c(1:4, 9:131)])
  #m_train_labels <- as.matrix(train_ct[1:nrow(train_ct), c(4)])
  
  print(train_ct)
  rf <- randomForest(LKADT_P ~ ., data=train_ct ,  na.action=na.roughfix)
  

  
}