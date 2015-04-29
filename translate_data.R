translate_data <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs){
  

  train_ct[, is.na(train_ct$TARGET)] <- "N"
  

}