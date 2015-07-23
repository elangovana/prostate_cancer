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
  
  
 
  
  source("./alg_random_forest.R")
  aligned_data <- setup_data_ttl(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  df_predicted_ttl_all <- NULL
  for(i in 1:5){
    temp_out= file.path(out_dir, paste("iteration_ttl", i, sep="_"))
    dir.create(temp_out)
    model_ttl <- predict_timetolive(subset_train, subset_test,dependent_variables, temp_out)
    df_predicted_ttl_temp <- as.data.frame(model_ttl$predictions, row.names=names(model_ttl$predictions))
    colnames(df_predicted_ttl_temp) <- c(paste("LKADT_P", i, sep="_"))   
    df_predicted_ttl <- df_predicted_ttl_temp
    if (is.null(df_predicted_ttl_all)){
      df_predicted_ttl_all <- df_predicted_ttl_temp
    }
    else{
      df_predicted_ttl_all <- merge.data.frame(df_predicted_ttl_all, df_predicted_ttl_temp, by = 0)
      rownames(df_predicted_ttl_all) <- df_predicted_ttl_all$Row.names
      df_predicted_ttl_all <- df_predicted_ttl_all[, !colnames(df_predicted_ttl_all) %in% c("Row.names") ]
    }
  }
  df_predicted_ttl_all$LKADT_P <- apply(df_predicted_ttl_all, 1, mean)
  write.csv(df_predicted_ttl_all, file=file.path(out_dir, "iter_predictions_ttl.csv"))
  write.csv( data.frame(RPT=rownames(df_predicted_ttl_all), TIMETOEVENT=df_predicted_ttl_all$LKADT_P), file=file.path(out_dir, "submission_1b.csv"), row.names=FALSE)  
  df_predicted_ttl <- df_predicted_ttl_all
  
  
    
  #predict death
  #merge parts of data into one 
  aligned_data <-setup_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)    
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  model_death <- predict_death(subset_train, subset_test, dependent_variables, out_dir)
  df_predicted_death <- as.data.frame(model_death$predictions, row.names=names(model_death$predictions))
  colnames(df_predicted_death) <- c("DEATH")
  
 
  
  #merge all results into a single df
  df_predicted <- merge(df_predicted_ttl, df_predicted_death, by=0,  suffixes= c("ttl", "dth" ))  
  rownames(df_predicted) <- df_predicted$Row.names
  df_predicted <- df_predicted[, !colnames(df_predicted) %in% c("Row.names")]
  
  #run risk scores
#   score_12 = calculate_risk_score(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs,  12 * 30, out_dir)
#   score_18 = calculate_risk_score(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, 18 * 30, out_dir)
#   score_24 = calculate_risk_score(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, 24 * 30, out_dir)
#   score_global = calculate_risk_score(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, 6 * 30, out_dir)
#   
  
#   score_12 = run_risk_score(model_ttl, model_death, "DEATH_IN_12MONTHS", out_dir)
#   score_18 = run_risk_score(model_ttl, model_death, "DEATH_IN_18MONTHS", out_dir)
#   score_24 = run_risk_score(model_ttl, model_death, "DEATH_IN_24MONTHS", out_dir)
#   score_global = run_risk_score(model_ttl, model_death, "DEATH_IN_GLOBAL", out_dir)
  
  #get ready for submission
  #score_all <- calculate_risk_score_for_all(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
score_all <- calculate_risk_score_lassocox(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
score_12 <-score_all$score_12
score_18 <-score_all$score_18
score_24 <-score_all$score_24
score_global <-score_all$score_global
  risk_score_global <- score_global$test
  risk_score_12 <- score_12$test
  risk_score_18 <- score_18$test
  risk_score_24 <- score_24$test
  df_submission_q1a <- data.frame(RPT=names(risk_score_global), riskScoreGlobal=risk_score_global, riskScore12=risk_score_12[names(risk_score_global)], riskScore18=risk_score_18[names(risk_score_global)], riskScore24=risk_score_24[names(risk_score_global)])
  write.csv(df_submission_q1a, file=file.path(out_dir,"submission_q1a.csv" ), row.names=FALSE)
  
  return (list(df_predicted=df_predicted, model_death= model_death, model_ttl= model_ttl, risk_score_12=score_12,risk_score_24=score_24, risk_score_18=score_18, risk_score_global=score_global ) )
}


calculate_risk_score_custom <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  calc_dummy_score <- function(row, days, lowerdays=0){
    #   # return(as.numeric(row["LKADT_P"]))
    #    if (is.na(row["DEATH"]))  {return(as.numeric(row["LKADT_P"]))}
    #    if (as.character(row["DEATH"]) =="YES") {return(as.numeric(row["LKADT_P"] )+ 50000)}
    #    return(as.numeric(row["LKADT_P"]))
    
    result = 0
    if(is.na(row["DEATH"])){
      result = 0
    }
    else if (as.character(row["DEATH"]) =="YES"){
      if((as.numeric(row["LKADT_P"]) >= lowerdays) & (as.numeric(row["LKADT_P"]) < days)) {result = 1/as.numeric(row["LKADT_P"])}
      else{result = -1 * as.numeric(row["LKADT_P"])}
    }
    return(result)
  }
  
  flog.info("Running function calculate_risk_score_custom for all")
  outdir = file.path(outdir, paste("calc_risk_score_custom", sep="_" ))
  dir.create(outdir)
  # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct,  train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_test_data = merge_all_data(test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)
  
  aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir) 
  predictors_ttl <- get_predictors_for_ttl(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir, topnpercent=.50)   
  predictors_death <- get_predictors_for_death(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)
 
  test <- predictors_ttl$model_data$test
  test$DEATH <- predictors_death$model_data$predictions[rownames(test)]
  test$LKADT_P <- predictors_ttl$model_data$predictions[rownames(test)]
  
  
  result_12 = apply(test,1, function(row){
    calc_dummy_score ( row, 12*30.5 )
  })
  result_18 = apply(test,1, function(row){
    calc_dummy_score ( row, 18*30.5 )
  })
  result_24 = apply(test,1, function(row){
    calc_dummy_score ( row, 24*30.5 )
  })
  result_global =apply(test,1, function(row){
    calc_dummy_score ( row, 30*30.5, 6*30.5 )
  })
  
  
  flog.info("Completed function calculate_risk_score for all for time period")
  return(list(score_12=list(test=result_12, train=result_12), score_18=list(test=result_18, train=result_18), score_24=list(test=result_24, train=result_24), score_global= list(test=result_global, train=result_global)))
}

calculate_risk_score_death <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  calc_dummy_score <- function(row, days, lowerdays=0){
    #   # return(as.numeric(row["LKADT_P"]))
    #    if (is.na(row["DEATH"]))  {return(as.numeric(row["LKADT_P"]))}
    #    if (as.character(row["DEATH"]) =="YES") {return(as.numeric(row["LKADT_P"] )+ 50000)}
    #    return(as.numeric(row["LKADT_P"]))
    
    result = 0
    if(is.na(row["DEATH"])){
      result = 0
    }
    else if (as.character(row["DEATH"]) =="YES"){
      if((as.numeric(row["LKADT_P"]) >= lowerdays) & (as.numeric(row["LKADT_P"]) < days)) {result = 1/as.numeric(row["LKADT_P"])}
      else{result = -1 * as.numeric(row["LKADT_P"])}
    }
    return(result)
  }
  
  flog.info("Running function calculate_risk_score_custom for all")
  outdir = file.path(outdir, paste("calc_risk_score_custom", sep="_" ))
  dir.create(outdir)
  # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct,  train_lv, data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  merged_test_data = merge_all_data(test_ct, test_lv, data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  
  aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir) 
  predictors_ttl <- get_predictors_for_ttl(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir, topnpercent=.50)   
  predictors_death <- get_predictors_for_death(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)
  
  test <- predictors_ttl$model_data$test
  test$DEATH <- predictors_death$model_data$predictions[rownames(test)]
  test$LKADT_P <- predictors_ttl$model_data$predictions[rownames(test)]
  
  
  result_12 = apply(test,1, function(row){
    calc_dummy_score ( row, 12*30.5 )
  })
  result_18 = apply(test,1, function(row){
    calc_dummy_score ( row, 18*30.5 )
  })
  result_24 = apply(test,1, function(row){
    calc_dummy_score ( row, 24*30.5 )
  })
  result_global =apply(test,1, function(row){
    calc_dummy_score ( row, 30*30.5, 6*30.5 )
  })
  
  
  flog.info("Completed function calculate_risk_score for all for time period")
  return(list(score_12=list(test=result_12, train=result_12), score_18=list(test=result_18, train=result_18), score_24=list(test=result_24, train=result_24), score_global= list(test=result_global, train=result_global)))
}

calculate_risk_score_lassocox <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
  test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
    flog.info("Running function calculate_risk_score for time period ")
    outdir = file.path(outdir, "calc_risk_score_lasso_forall")
    dir.create(outdir)
    # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
    merged_train_data = merge_all_data(train_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
    merged_test_data = merge_all_data(test_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
        
    aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir) 
    predictors <- get_predictors_for_ttl(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)   
    #predictors <- get_predictors_for_death(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)
   
    train <- predictors$model_data$train
    test <- predictors$model_data$test
    
    result_12 = run_risk_score_lassocox(train,test, predictors$predictors, 12*30, outdir)
  #  result_18 = run_risk_score_lassocox(train,test, predictors$predictors, 18*30, outdir)
   # result_24 = run_risk_score_lassocox(train,test, predictors$predictors, 24*30, outdir)
  #  result_global = run_risk_score_lassocox(train,test, predictors$predictors, 0, outdir)
    
    
    flog.info("Completed function calculate_risk_score for time period")
    return(list(score_12=result_12, score_18=result_12, score_24=result_12, score_global= result_12))
    
   
}

calculate_risk_score_for_all_2 <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  flog.info("Running function calculate_risk_score_using_model for all")
  outdir = file.path(outdir, paste("calc_risk_score_all", sep="_" ))
  dir.create(outdir)
  # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct, train_lv, data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  merged_test_data = merge_all_data(test_ct, test_lv, data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  
  aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir) 
  predictors <- get_predictors_for_ttl(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)   
  #predictors_death <- get_predictors_for_death(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)
  
  train <- predictors$model_data$train
  test <- predictors$model_data$test
  
  
#   result_12 = run_risk_score_lassocox(train,test, predictors$predictors, 12*30, outdir)
#   result_18 = run_risk_score_lassocox(train,test, predictors$predictors, 18*30, outdir)
#   result_24 = run_risk_score_lassocox(train,test, predictors$predictors, 24*30, outdir)
#   result_global = run_risk_score_lassocox(train,test, predictors$predictors, 0, outdir)

  result_12 = run_risk_score(train,test, predictors$predictors, 12*30, outdir)
  result_18 = run_risk_score(train,test, predictors$predictors, 18*30, outdir)
  result_24 = run_risk_score(train,test, predictors$predictors, 24*30, outdir)
  result_global = run_risk_score(train,test, predictors$predictors, 0, outdir)


  flog.info("Completed function calculate_risk_score for all for time period")
  return(list(score_12=result_12, score_18=result_18, score_24=result_24, score_global= result_global))
  
}

calculate_risk_score_for_all <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                         test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  flog.info("Running function calculate_risk_score_using_model for all")
  outdir = file.path(outdir, paste("calc_risk_score_all", sep="_" ))
  dir.create(outdir)
  # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  merged_test_data = merge_all_data(test_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  
  aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir) 
  predictors_ttl <- get_predictors_for_ttl(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir, topnpercent=.50)   
  predictors_death <- get_predictors_for_death(aligned_data$train, aligned_data$test, aligned_data$dependent_variables, outdir)
  #predictors_to_use = Reduce(union, c("ALP", "HB", "PSA", "AST", "CA"), predictors_ttl$predictors)
  predictors_to_use = predictors_ttl$predictors#c("ALP", "HB", "PSA", "AST", "CA", "WBC", )
  test <- predictors_ttl$model_data$test
  test$DEATH <- predictors_death$model_data$predictions[rownames(test)]
  test$LKADT_P <- predictors_ttl$model_data$predictions[rownames(test)]
 
  #   result_12 = run_risk_score_lassocox(train,test, predictors$predictors, 12*30, outdir)
  #   result_18 = run_risk_score_lassocox(train,test, predictors$predictors, 18*30, outdir)
  #   result_24 = run_risk_score_lassocox(train,test, predictors$predictors, 24*30, outdir)
  #   result_global = run_risk_score_lassocox(train,test, predictors$predictors, 0, outdir)
  
  result_12 = run_risk_score(test,test, predictors_to_use, 12*30, outdir)
  #result_18 = run_risk_score(test,test, predictors_ttl$predictors, 18*30, outdir)
  #result_24 = run_risk_score(test,test, predictors_ttl$predictors, 24*30, outdir)
  #result_global = run_risk_score(test,test, predictors_ttl$predictors, 0, outdir)
  
  result_18 = result_12
  result_24 = result_12
  result_global = result_12
  flog.info("Completed function calculate_risk_score for all for time period")
  return(list(score_12=result_12, score_18=result_18, score_24=result_24, score_global= result_global))
  
}

get_predictors_for_ttl <- function(subset_train, subset_test,dependent_variables, out_dir, topnpercent=1.0){
  model <- predict_timetolive(subset_train, subset_test,dependent_variables, out_dir)
  imp_rf <- importance(model$fit)
  predictor_rating<- imp_rf[imp_rf[, "%IncMSE"] > 0, "IncNodePurity"]
  predictor_rating <- sort(predictor_rating, decreasing = TRUE)
  topn = ceiling(length(predictor_rating) * topnpercent) # only consider top n%
  flog.info("No of topN variables used for risk calculation : %s out of %s, out of full predictor %s, topnpercent= %s", topn, length(predictor_rating), nrow(imp_rf), topnpercent)    
  return(list(model_data=model, predictors=names(predictor_rating[c(1:topn)])))
}

get_predictors_for_death <- function(subset_train, subset_test,dependent_variables, out_dir, topnpercent=1){
  model <- predict_death(subset_train, subset_test,dependent_variables, out_dir)
  ##Obtain predictors for cox model.
  imp_rf <- importance(model$fit)
  predictor_rating<- imp_rf[imp_rf[, "MeanDecreaseAccuracy"] > 0, "MeanDecreaseGini"]
  predictor_rating <- sort(predictor_rating, decreasing = TRUE)
  topn = ceiling(length(predictor_rating) * topnpercent) # only consider top n%
  flog.info("No of topN variables used for risk calculation : %s out of %s, out of full predictor %s, topnpercent= %s", topn, length(predictor_rating), nrow(imp_rf), topnpercent)    
  return(list(model_data=model, predictors=names(predictor_rating[c(1:topn)])))
}

setup_data_ttl <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                    test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  #merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  merged_test_data = merge_all_data(test_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  
  flog.info("%s records out of %s removed from train for time to event, as they are censored",  length(which(as.character(merged_train_data$DEATH)!="YES")), nrow(merged_train_data))  
  subset_train_ttl <- merged_train_data[as.character(merged_train_data$DEATH)=="YES", ]
  
  aligned_data <- align_test_train_data(subset_train_ttl, merged_test_data, outdir)  
  return(aligned_data)
}

setup_data <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                           test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
 # merged_train_data = merge_all_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, outdir)
  merged_train_data = merge_all_data(train_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  merged_test_data = merge_all_data(test_ct, data.frame(), data.frame(), data.frame(), data.frame(), data.frame(), outdir)
  
 
  aligned_data <- align_test_train_data(merged_train_data, merged_test_data, outdir)  
  return(aligned_data)
}

ml_pipeline_part2 <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
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
  
  
  
  
  source("./alg_random_forest.R")
 
  
  #predict death
  #merge parts of data into one 
  aligned_data <-setup_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs, test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)    
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

merge_all_data <- function(df_ct, df_lv, df_lm, df_mh, df_pm, df_vs, outdir){
  ## Merge all med information from multiple datasets into one large wide dataset
  # merge train core table with lab value
  #df_ct <- df_ct[,  c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC")]
  df_subset_merged <- merge(df_ct, df_lv, by=0, all.x=TRUE, suffixes= c(".ct", ".lv" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))    
  #df_subset_merged <- df_ct[, c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC","PSA", "HB", "BONE", "ALB", "ALP", "LDH", "LYMPH_NODES", "ECOG_C", "ANALGESICS", "GLUCOCORTICOID", "ESTROGENS", "TESTO")]
  #df_subset_merged<-df_ct
  
  #merge Lesion measure
  df_subset_merged <- merge(df_subset_merged, df_lm, by=0, all.x=TRUE, suffixes= c(".ctlv", ".lm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #   #merge medical history
  df_subset_merged <- merge(df_subset_merged, df_mh, by=0, all.x=TRUE, suffixes= c(".ctlvlm", ".mh" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  #   
  #   #merge vital signs
  df_subset_merged <- merge(df_subset_merged, df_vs, by=0, all.x=TRUE, suffixes= c(".ctlvlmmh", ".vs" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #   
  #   #merge prior medications
  df_subset_merged <- merge(df_subset_merged, df_pm, by=0, all.x=TRUE, suffixes= c(".ctlvlmmhpm", ".pm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  
  return(df_subset_merged)
  
}
align_test_train_data<- function(train, test, outdir){
  library(futile.logger)
  
  subset_train <- train  
  dependent_variables = dependent_variables()
  subset_test <- test[, !colnames(test) %in% c("DOMAIN" , "STUDYID")]
  
  #remove columns with all NA
  subset_train <-remove_all_nan_columns( remove_all_na_columns(subset_train))
  subset_test <- remove_all_nan_columns(remove_all_na_columns(subset_test))
  
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  commonCols  <- commonCols[!commonCols %in% c("DOMAIN", "STUDYID")]
  subset_train <- subset_train[, Reduce(union, commonCols, dependent_variables)]
  subset_test <- subset_test[, commonCols]
  
  
  datasets_aligned_factors <-  make_factors_alike(subset_train, subset_test)
  
  
  return(list(train = datasets_aligned_factors$train, test=datasets_aligned_factors$test, dependent_variables = dependent_variables))
}