ml_pipeline <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
  
  #clean training 
  print("cleaning train")
  train_ct <- clean_ct_data (train_ct)
  train_ct <- clean_labels (train_ct)
  train_lv <- clean_labvalue_data (train_lv)
  train_lm <- clean_lesionmeasure_data(train_lm)
  train_mh <- clean_medical_history(train_mh)
  
  #clean test
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)  
  test_lv <- clean_labvalue_data (test_lv)
  test_lm <- clean_lesionmeasure_data(test_lm)
  test_mh <- clean_medical_history(test_mh)
  
  source("./alg_random_forest.R")
  model_ttl <- predict_timetolive(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                     test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted_ttl <- as.data.frame(model_ttl$predictions, row.names=names(model_ttl$predictions))
  colnames(df_predicted_ttl) <- c("LKADT_P")
  
  model_death <- predict_death(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted_death <- as.data.frame(model_death$predictions, row.names=names(model_death$predictions))
  colnames(df_predicted_death) <- c("DEATH")
  
  #merge all results into a single df
  df_predicted <- merge(df_predicted_ttl, df_predicted_death, by=0,  suffixes= c("ttl", "dth" ))  
  rownames(df_predicted) <- df_predicted$Row.names
  df_predicted <- df_predicted[, !colnames(df_predicted) %in% c("Row.names")]
  
  #run risk scores
  score_12 = run_risk_score(model_ttl, model_death, 12, out_dir)
  score_18 = run_risk_score(model_ttl, model_death, 18, out_dir)
  score_24 = run_risk_score(model_ttl, model_death, 24, out_dir)
  score_global = run_risk_score(model_ttl, model_death, 0, out_dir)
  
  #get ready for submission
  risk_score_global <- score_global$test$fit
  risk_score_12 <- score_12$test$fit
  risk_score_18 <- score_18$test$fit
  risk_score_24 <- score_24$test$fit  
  df_submission_q1a <- data.frame(RPT=names(risk_score_global), riskScoreGlobal=risk_score_global, riskScore12=risk_score_12[names(risk_score_global)], riskScore18=risk_score_18[names(risk_score_global)], riskScore24=risk_score_24[names(risk_score_global)])
  write.csv(df_submission_q1a, file=file.path(out_dir,"submission_q1a.csv" ), row.names=FALSE)
  
  return (list(df_predicted=df_predicted, model_death= model_death, model_ttl= model_ttl, risk_score_12=score_12,risk_score_24=score_24, risk_score_18=score_18, risk_score_global=score_global ) )
}