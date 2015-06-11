ml_pipeline <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
 
  #clean training 
  
  flog.info("cleaning train")
  flog.info("No of train records before clean up %s",nrow(train_ct) )
  train_ct <- clean_ct_data (train_ct)
  train_ct <- clean_labels (train_ct)
  train_lv <- clean_labvalue_data (train_lv)
  train_lm <- clean_lesionmeasure_data(train_lm)
  train_mh <- clean_medical_history(train_mh)
  train_vs <- clean_vital_signs(train_vs)
  train_pm <- clean_prior_medicals(train_pm)
  flog.info("No of train records after clean up %s",nrow(train_ct) )
  
  #clean test
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)  
  test_lv <- clean_labvalue_data (test_lv)
  test_lm <- clean_lesionmeasure_data(test_lm)
  test_mh <- clean_medical_history(test_mh)
  test_vs <- clean_vital_signs(test_vs)
  test_pm <- clean_prior_medicals(test_pm)
  
  
  #merge parts of data into one
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  source("./alg_random_forest.R")
  #for TTL use data where death is true only, not censored data
  flog.info("%s records out of %s removed from train for time to event, as they are censored",  length(which(as.character(subset_train$DEATH)!="YES")), nrow(subset_train))
  subset_train_ttl <- subset_train[as.character(subset_train$DEATH)=="YES", ]

  model_ttl <- predict_timetolive(subset_train_ttl, subset_test,dependent_variables, out_dir)
  df_predicted_ttl <- as.data.frame(model_ttl$predictions, row.names=names(model_ttl$predictions))
  colnames(df_predicted_ttl) <- c("LKADT_P")
  
  #predict_ttl_lasso(subset_train, subset_test,dependent_variables, out_dir)
  
  model_death <- predict_death(subset_train, subset_test, dependent_variables, out_dir)
  df_predicted_death <- as.data.frame(model_death$predictions, row.names=names(model_death$predictions))
  colnames(df_predicted_death) <- c("DEATH")
  
 
  
  #merge all results into a single df
  df_predicted <- merge(df_predicted_ttl, df_predicted_death, by=0,  suffixes= c("ttl", "dth" ))  
  rownames(df_predicted) <- df_predicted$Row.names
  df_predicted <- df_predicted[, !colnames(df_predicted) %in% c("Row.names")]
  
  #run risk scores
  score_12 = run_risk_score(model_ttl, model_death, 12 * 30, out_dir)
  score_18 = run_risk_score(model_ttl, model_death, 18 * 30, out_dir)
  score_24 = run_risk_score(model_ttl, model_death, 24 * 30, out_dir)
  score_global = run_risk_score(model_ttl, model_death, 0, out_dir)
  
  
#   score_12 = run_risk_score(model_ttl, model_death, "DEATH_IN_12MONTHS", out_dir)
#   score_18 = run_risk_score(model_ttl, model_death, "DEATH_IN_18MONTHS", out_dir)
#   score_24 = run_risk_score(model_ttl, model_death, "DEATH_IN_24MONTHS", out_dir)
#   score_global = run_risk_score(model_ttl, model_death, "DEATH_IN_GLOBAL", out_dir)
  
  #get ready for submission
  risk_score_global <- score_global$test$fit
  risk_score_12 <- score_12$test$fit
  risk_score_18 <- score_18$test$fit
  risk_score_24 <- score_24$test$fit  
  df_submission_q1a <- data.frame(RPT=names(risk_score_global), riskScoreGlobal=risk_score_global, riskScore12=risk_score_12[names(risk_score_global)], riskScore18=risk_score_18[names(risk_score_global)], riskScore24=risk_score_24[names(risk_score_global)])
  write.csv(df_submission_q1a, file=file.path(out_dir,"submission_q1a.csv" ), row.names=FALSE)
  
  return (list(df_predicted=df_predicted, model_death= model_death, model_ttl= model_ttl, risk_score_12=score_12,risk_score_24=score_24, risk_score_18=score_18, risk_score_global=score_global ) )
}


ml_pipeline_part2 <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
  
  #clean training 
  print("cleaning train")
  train_ct <- clean_ct_data (train_ct)
  train_ct <- clean_labels (train_ct)
  train_lv <- clean_labvalue_data (train_lv)
  train_lm <- clean_lesionmeasure_data(train_lm)
  train_mh <- clean_medical_history(train_mh)
  train_vs <- clean_vital_signs(train_vs)
  train_pm <- clean_prior_medicals(train_pm)
  
  #clean test
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)  
  test_lv <- clean_labvalue_data (test_lv)
  test_lm <- clean_lesionmeasure_data(test_lm)
  test_mh <- clean_medical_history(test_mh)
  test_vs <- clean_vital_signs(test_vs)
  test_pm <- clean_prior_medicals(test_pm)
  
  #merge parts of data into one
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  source("./alg_random_forest.R")
  model_discontinuedflag  <- predict_discontinuedflag(subset_train, subset_test,dependent_variables, out_dir)
  df_predicted_discontinuedflag <- as.data.frame(model_discontinuedflag$predictions, row.names=names(model_discontinuedflag$predictions))
  colnames(df_predicted_discontinuedflag) <- c("DISCONT")
  
 
  
  return (list(df_predicted=df_predicted_discontinuedflag, model_discontinuedflag= model_discontinuedflag ) )
}

ml_pipeline_part3 <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir){ 
  source("./translate_data.R")
  
  #clean training 
  print("cleaning train")
  train_ct <- clean_ct_data (train_ct)
  train_ct <- clean_labels (train_ct)
  train_lv <- clean_labvalue_data (train_lv)
  train_lm <- clean_lesionmeasure_data(train_lm)
  train_mh <- clean_medical_history(train_mh)
  train_vs <- clean_vital_signs(train_vs)
  train_pm <- clean_prior_medicals(train_pm)
  
  #clean test
  print("cleaning core table for test")
  test_ct <- clean_ct_data(test_ct)  
  test_lv <- clean_labvalue_data (test_lv)
  test_lm <- clean_lesionmeasure_data(test_lm)
  test_mh <- clean_medical_history(test_mh)
  test_vs <- clean_vital_signs(test_vs)
  test_pm <- clean_prior_medicals(test_pm)
  
  #merge parts of data into one
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  #run ml alg
  source("./alg_random_forest.R")
  model_discontinuedreason  <- predict_discontinuedreason(subset_train, subset_test,dependent_variables, out_dir)
  df_predicted_discontinuedreason <- as.data.frame(model_discontinuedreason$predictions, row.names=names(model_discontinuedreason$predictions))
  colnames(df_predicted_discontinuedreason) <- c("ENDTRS_C")
  
  
  
  return (list(df_predicted=df_predicted_discontinuedreason, model_discontinuedreason= model_discontinuedreason ) )
}
