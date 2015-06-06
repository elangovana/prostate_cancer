merge_all_data <- function(df_ct, df_lv, df_lm, df_mh, df_pm, df_vs){
  ## Merge all med information from multiple datasets into one large wide dataset
  # merge train core table with lab value
  df_subset_merged <- merge(df_ct, df_lv, by=0, all.x=TRUE, suffixes= c(".ct", ".lv" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #merge Lesion measure
  df_subset_merged <- merge(df_subset_merged, df_lm, by=0, all.x=TRUE, suffixes= c(".ctlv", ".lm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #merge medical history
  df_subset_merged <- merge(df_subset_merged, df_mh, by=0, all.x=TRUE, suffixes= c(".ctlvlm", ".mh" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #merge vital signs
  df_subset_merged <- merge(df_subset_merged, df_vs, by=0, all.x=TRUE, suffixes= c(".ctlvlmmh", ".vs" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  
  #merge prior medications
  df_subset_merged <- merge(df_subset_merged, df_pm, by=0, all.x=TRUE, suffixes= c(".ctlvlmmhpm", ".pm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))

  
  return(df_subset_merged)
  
}

align_test_train_data<- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                 test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  #remove other labels, except for the label LKADT_P that is predicted, from the train set  
  subset_train_ct <- train_ct
  
  dependent_variables = c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC")
  
  
  subset_train <- merge_all_data(subset_train_ct, train_lv, train_lm, train_mh, train_pm, train_vs)
  #merge test, remove domain study columns as they are duplicates
  subset_test <- test_ct[, !colnames(test_ct ) %in% c("DOMAIN" , "STUDYID")]
  subset_test <- merge_all_data(subset_test, test_lv, test_lm, test_mh, test_pm, test_vs)
  
  
  
  #remove columns with all NA
  subset_train <- subset_train[,colSums(is.na(subset_train))<nrow(subset_train)]
  subset_test <- subset_test[, colSums(is.na(subset_test))<nrow(subset_test)]
  
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  commonCols  <- commonCols[!commonCols %in% c("DOMAIN", "STUDYID")]
  #commonCols <- commonCols[1:6]
  subset_train <- subset_train[, Reduce(union, commonCols, dependent_variables)]
  subset_test <- subset_test[, commonCols]
  
  for(c in commonCols){
    if (typeof(subset_train[, c(c)]) != typeof(subset_test[, c(c)])){
      warning(paste ("The type of", c, "train", typeof(subset_train[, c(c)]) , "does not match the type in test", typeof(subset_test[, c(c)])))
    }
    
    if (is.factor(subset_train[, c(c)])){
      levels_in_test_and_train= Reduce(union, as.character(unique(subset_train[, c(c)])), as.character(unique(subset_test[, c(c)])))
      levels_missing_in_test = setdiff(levels(subset_train[, c(c)]), levels(subset_test[, c(c)]))
      levels_missing_in_train= setdiff(levels(subset_test[, c(c)]), levels(subset_train[, c(c)]))
      
      if (length(levels_missing_in_train) > 0){        
        levels(subset_train[, c(c)]) <- c(levels(subset_train[, c(c)]), as.character(levels_missing_in_train))
      }
      if (length(levels_missing_in_test) > 0){       
        levels(subset_test[, c(c)]) <- c(levels(subset_test[, c(c)]), as.character(levels_missing_in_test))
      }   
    }
    
  }
  
  
  print("-----TRAIN DATA SET STRUCTURE AFTER CLEAN UP-------")
  print(str(subset_train,list.len = 2999 ))
  print("------TEST DATA SET STRUCTURE AFTER CLEAN UP------")
  print(str(subset_test,list.len = 2999 ))
  
  return(list(train =subset_train, test= subset_test, dependent_variables = dependent_variables))
}

predict_timetolive <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                               test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  library("randomForest")
  print("----begin function predict_timetolive---")
  
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)
  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  #run RF, use rough fix for missing values
  subset_train.roughfix <- na.roughfix(subset_train)
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$LKADT_P, ntree = 1000,  importance=TRUE)
  
  write.csv( importance(fit) , file=file.path(outdir, "timetolive_importanceFit.csv"))  
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )  
  print(paste ("RMSE:", rmse, sep=" "))
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "timetolive_predictedtraining.csv"))
  
  subset_test.roughfix <- na.roughfix(subset_test)
  predictions = predict(fit, subset_test.roughfix )  
  write.csv( predictions, file=file.path(outdir, "timetolive_predictedtest.csv"))
  write.csv( data.frame(RPT=names(predictions), TIMETOEVENT=predictions), file=file.path(outdir, "submission_1b.csv"), row.names=FALSE)
  
  ##Return model
  print("----end function predict_timetolive---")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}

predict_death <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                          test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  
  print("----Begin function predict_death---")
  library("randomForest")
  
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)
  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  #Run RF
  subset_train.roughfix <- na.roughfix(subset_train)
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$DEATH, ntree = 1000,  importance=TRUE)    
  write.csv( importance(fit) , file=file.path(outdir, "death_importanceFit.csv"))  
  
  percentageCorrect =   (length(subset_train$DEATH[subset_train$DEATH == fit$predicted]) / nrow(subset_train) ) * 100  
  print(paste("Percentage correct:", percentageCorrect, " "))
  
  
  #Write predicted train data
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "death_predictedtraining.csv"))
  
  #run model on test
  subset_test.roughfix <- na.roughfix(subset_test)
  predictions = predict(fit, subset_test.roughfix )
  write.csv( predictions, file=file.path(outdir, "death_predictedtest.csv"))
  
  
  ##Return model
  print("----End function predict_death---")
  return(list(predictions=predictions, fit=fit , train=subset_train.roughfix, test=subset_test.roughfix ))  
}

calc_event <- function(time_period_in_months, row){
  
  #if censored no event
  if (row["DEATH"] == "CENSORED"){
    return(FALSE)
  }
  # for time agnostic calc, the event occurs for if not cenceored
  if (time_period_in_months <= 0){
    return(TRUE)
  }
  ##the event has occurred
  #If the number of months less than time period, even has occurred
  if ( as.numeric(row["LKADT_P"])/30 <= time_period_in_months){
    return(TRUE)
  }
  #no event
  return (FALSE)
}


run_risk_score <- function(model_ttl, model_death, time_period_in_months, outdir){
  
  #apply time period on event
  df_train <- model_ttl$train
  df_train$EVENT <- apply(df_train, 1, function(x){
    calc_event(time_period_in_months, x)
  })
  
  #apply time period on event for test  
  df_test <- model_ttl$test
  death_predictions <- model_death$predictions
  ttl_predictions <- model_ttl$predictions
  
  
  df_test$DEATH <- death_predictions[rownames(df_test)]
  df_test$LKADT_P <-  ttl_predictions[rownames(df_test)]
  
  
  
  df_test$EVENT <- apply(df_test, 1, function(x){
    calc_event(time_period_in_months, x)
  })
  
  ##Obtain predictors for cox model.
  predictor_rating<- importance(model_ttl$fit)[, "%IncMSE"]
  predictor_rating <- sort(predictor_rating, decreasing = TRUE)
  topn = ceiling(length(predictor_rating[which(predictor_rating>0)]) * .25) # only consider top 25%
  print("topn length")
  print(length(predictor_rating))
  print(topn)
  formula = as.formula(paste("Surv(LKADT_P, EVENT)" , paste(names(predictor_rating[c(1:topn)]),collapse="+"), sep=" ~ "))
  print(formula)
  library(survival)
  cox_fit = coxph(formula, df_train)
  print("Dim cox fit")
  print(cox_fit)
  
  #calc score
  risk_scores_test = predict(cox_fit,type="risk",se.fit=TRUE, newdata = df_test)
  risk_scores_train = predict(cox_fit,type="risk",se.fit=TRUE, newdata = df_train)
  
  
  #write output
  write.csv( risk_scores_test, file=file.path(outdir, paste("risk_scores_test",  time_period_in_months,".csv", sep="")))
  write.csv( risk_scores_train, file=file.path(outdir, paste("risk_scores_train", time_period_in_months ,".csv", sep="")))
  
  return(list(train=risk_scores_train, test =risk_scores_test ))
  #print(df_train[, c("risk_score", names(predictor_rating[c(1:topn)]))])
  
}

predict_discontinuedflag <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                     test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  library("randomForest")
  print("----Begin function predict_discontinuedflag---")
  
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)
  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  #run RF, use rough fix for missing values
  
  subset_train.roughfix <- na.roughfix(subset_train)
  subset_test.roughfix <- na.roughfix(subset_test)
  
   
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$DISCONT, ntree = 1000,  importance=TRUE)
  
  write.csv( importance(fit) , file=file.path(outdir, "discontinuedflag_importanceFit.csv"))  
  
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "discontinuedflag_predictedtraining.csv"))
  
  
  predictions = predict(fit, subset_test.roughfix )  
  write.csv( predictions, file=file.path(outdir, "discontinuedflag_predictedtest.csv"))
  #write.csv( data.frame(RPT=names(predictions), TIMETOEVENT=predictions), file=file.path(outdir, "discontinuedflag_1b.csv"), row.names=FALSE)
  
  ##Return model
  print("----End function predict_discontinuedflag---")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}


predict_discontinuedreason <- function(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                     test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir){
  library("randomForest")
  library("futile.logger")
  flog.info("Begin function predict_discontinuedreason")
  
  aligned_data <- align_test_train_data(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, outdir)
  
  subset_train <- aligned_data$train
  subset_test <- aligned_data$test
  dependent_variables <- aligned_data$dependent_variables
  
  #run RF, use rough fix for missing values
  
  subset_train.roughfix <- na.roughfix(subset_train)
  subset_test.roughfix <- na.roughfix(subset_test)
  
  flog.info("Running random forest")
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$ENDTRS_C, ntree = 1000,  mtry=30, importance=TRUE)
  flog.info("Random forest fit")
  flog.info("  ntree %s", fit$ntree)
  flog.info("  mtry %s", fit$mtry)
  flog.info(" Error rate:")
  flog.info(fit$err.rate)
  write.csv( importance(fit) , file=file.path(outdir, "discontinuedreason_importanceFit.csv"))  
  
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train,train_predicted_df, by=0) , file=file.path(outdir, "discontinuedreason_predictedtraining.csv"))
  
  
  predictions = predict(fit, subset_test.roughfix )  
  write.csv( predictions, file=file.path(outdir, "discontinuedreason_predictedtest.csv"))
  #write.csv( data.frame(RPT=names(predictions), TIMETOEVENT=predictions), file=file.path(outdir, "discontinuedflag_1b.csv"), row.names=FALSE)
  
  ##Return model
  flog.info("End function predict_discontinuedreason")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}