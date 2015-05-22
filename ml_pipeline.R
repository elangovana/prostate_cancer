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
  model_ttl <- predict_timetolive(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                     test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted_ttl <- model_ttl$predictions
  
  model_death <- predict_death(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted_death <- model_death$predictions

  #merge all results into a single df
  df_predicted <- merge(df_predicted_ttl, df_predicted_death, by=0,  suffixes= c("ttl", "dth" ))  
  rownames(df_predicted) <- df_predicted$Row.names
  df_predicted <- df_predicted[, !colnames(df_predicted) %in% c("Row.names")]
  
  #run risk scores
  score_12 = run_risk_score(model_ttl, model_death, 12, out_dir)
  score_18 = run_risk_score(model_ttl, model_death, 18, out_dir)
  score_24 = run_risk_score(model_ttl, model_death, 24, out_dir)
  return (df_predicted)
}