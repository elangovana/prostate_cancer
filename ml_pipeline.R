ml_pipeline <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
  
  #clean training 
  print("cleaning train")
  train_ct <- clean_ct_data (train_ct)
  train_ct <- clean_labels (train_ct)
  train_lv <- clean_labvalue_data (train_lv)
  train_lm <- clean_lesionmeasure_data(train_lm)
  
  #clean test
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)  
  test_lv <- clean_labvalue_data (test_lv)
  test_lm <- clean_lesionmeasure_data(test_lm)
  
  
  source("./alg_random_forest.R")
  df_predicted <- predict_timetolive(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                    test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  
#   df_predicted_death <- predict_death(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
#                      test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
#   
#   #merge all results into a single df
#   df_predicted <- merge(df_predicted_ttl, df_predicted_death, by=0,  suffixes= c("ttl", "dth" ))  
#   rownames(df_predicted) <- df_predicted$Row.names
#   df_predicted <- df_predicted[, !colnames(df_predicted) %in% c("Row.names")]
  return (df_predicted)
}