library(futile.logger)

predict_timetolive <- function(subset_train, subset_test, dependent_variables, outdir){
  library("randomForest")
  flog.info("Begin function predict_timetolive")
  log_datastructure(subset_train, subset_test)
  
  #run RF, use rough fix for missing values
  subset_train.roughfix <- subset_train
  subset_train.roughfix[, !colnames(subset_train.roughfix) %in% dependent_variables] <- na.roughfix(subset_train.roughfix[, !colnames(subset_train.roughfix) %in% dependent_variables])
  subset_test.roughfix <- na.roughfix(subset_test)  
  #Make sure the factor levels in train and test are the same, else random forest cracks!!
  datasets_aligned_factors <-  make_factors_alike(subset_train.roughfix, subset_test.roughfix)
  subset_train.roughfix <- datasets_aligned_factors$train
  subset_test.roughfix <- datasets_aligned_factors$test
  log_datastructure(subset_train.roughfix, subset_test.roughfix)
  
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$LKADT_P, ntree = 500, mtry=50, importance=TRUE, do.trace=TRUE)
  print("random forest fit: ")
  print(fit)
  
  write.csv( importance(fit) , file=file.path(outdir, "timetolive_importanceFit.csv"))  
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )  
  print(paste ("RMSE:", rmse, sep=" "))
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train.roughfix,train_predicted_df, by=0) , file=file.path(outdir, "timetolive_predictedtraining.csv"))
  
  
  predictions = predict(fit, subset_test.roughfix )  
  write.csv( predictions, file=file.path(outdir, "timetolive_predictedtest.csv"))
  write.csv( data.frame(RPT=names(predictions), TIMETOEVENT=predictions), file=file.path(outdir, "submission_1b.csv"), row.names=FALSE)
  
  ##Return model
  flog.info("End function predict_timetolive")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}

predict_ttl_lasso <- function(subset_train, subset_test, dependent_variables, outdir){
  
  #run RF, use rough fix for missing values
  subset_train.roughfix <- na.roughfix(subset_train)
  subset_test.roughfix <- na.roughfix(subset_test)  
  
  nums <- sapply(subset_train.roughfix, is.numeric)
  subset_train.roughfix <- subset_train.roughfix[ , nums]
  
  nums <- sapply(subset_test.roughfix, is.numeric)
  subset_test.roughfix <- subset_test.roughfix[ , nums]
  
  library(fastcox)
  x <- as.matrix(subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables])
  y= as.matrix(subset_train$LKADT_P)
  d=ifelse(as.character(subset_train$DEATH)=="YES",1,0)
  
  fit<-cocktail(x=x,y=y,d=d, alpha=0.5)
 
  print("random forest fit: ")
  print(fit)
  
   
  rmse = sqrt( sum( (subset_train$LKADT_P - fit$predicted)^2 , na.rm = TRUE ) / nrow(subset_train) )  
  print(paste ("RMSE:", rmse, sep=" "))
  
  #Write predicted train output 
  #set up predicted train as df
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train.roughfix,train_predicted_df, by=0) , file=file.path(outdir, "lass_cox_timetolive_predictedtraining.csv"))
  
  
  predictions = predict(fit, as.matrix(subset_test.roughfix), type="link" ) 
  print("Fit predicted")
  print(fit$predicted)
  
  print("test predicted")
  print(predictions)
  
  write.csv( predictions, file=file.path(outdir, "timetolive_predictedtest.csv"))
  write.csv( data.frame(RPT=names(predictions), TIMETOEVENT=predictions), file=file.path(outdir, "lass_cox_submission_1b.csv"), row.names=FALSE)
  
  ##Return model
  flog.info("End function predict_timetolive")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}

predict_death <- function(subset_train, subset_test, dependent_variables, outdir, predict_col ="DEATH"){
  
  flog.info("Begin function predict_death")
  library("randomForest")
  

  #run RF, use rough fix for missing values
  subset_train.roughfix <- subset_train

  subset_train.roughfix[, !colnames(subset_train.roughfix) %in% dependent_variables] <- na.roughfix(subset_train.roughfix[, !colnames(subset_train.roughfix) %in% dependent_variables])
  
  subset_test.roughfix <- na.roughfix(subset_test)  
  #Make sure the factor levels in train and test are the same, else random forest cracks!!
  datasets_aligned_factors <-  make_factors_alike(subset_train.roughfix, subset_test.roughfix)
  subset_train.roughfix <- datasets_aligned_factors$train
  subset_test.roughfix <- datasets_aligned_factors$test
  
  #Run RF
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix[, predict_col], ntree = 1000,  importance=TRUE)    
  write.csv( importance(fit) , file=file.path(outdir, paste(predict_col,"_importanceFit.csv")))  
  print("random forest fit: ")
  print(fit)
  
  percentageCorrect =   (nrow(subset_train[subset_train[,predict_col] == fit$predicted, ]) / nrow(subset_train) ) * 100  
  print(paste("Percentage correct: for", predict_col, percentageCorrect, " "))
  
  
  #Write predicted train data
  train_predicted_df <- data.frame(fit$predicted)
  rownames(train_predicted_df) <- names(fit$predicted);
  write.csv( merge(subset_train.roughfix,train_predicted_df, by=0) , file=file.path(outdir, paste (predict_col, "_predictedtraining.csv", sep="")))
  
  #run model on test
  predictions = predict(fit, subset_test.roughfix )
  write.csv( predictions, file=file.path(outdir, paste (predict_col, "_predictedtest.csv", sep="")))
  
  
  ##Return model
  flog.info("End function predict_death")
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
  if ( as.numeric(row["LKADT_P"]) > time_period_in_months){
    return(TRUE)
  }
  #no event
  return (FALSE)
}

run_risk_score2 <- function(model_ttl, model_death, time_period_in_days, outdir){
  
  add_time <- function(data, time_period_in_days){
    if (time_period_in_days == 0)
    {
      data[, "TIME"] <- data[, "LKADT_P"]
    }
    else
    {
      data[, "TIME"] <- time_period_in_days
    }
    return(data)
  }
  
  #create new dynamic column to predict death in days
  new_event_column_name <-  paste("DEATH_IN_", time_period_in_days, sep="")
  
  #apply  event for train
  df_train <- model_ttl$train 
  df_train <- add_label_death_within_days(df_train,time_period_in_days,  new_event_column_name)
 
  print(str(df_train, list.len=3000))
  #predict event for test  
  df_test <- model_ttl$test
  model_death_in_days <- predict_death(df_train, df_test, c(dependent_variables(), new_event_column_name), outdir, new_event_column_name)
  death_in_days_predictions <- model_death_in_days$predictions
  df_test$LKADT_P <-  model_ttl$predictions[rownames(df_test)]
  
  #add time
  df_train <- add_time(df_train, time_period_in_days)
  df_test <- add_time(df_test, time_period_in_days)
  #add event
  df_test$EVENT <-  as.character(death_in_days_predictions[rownames(df_test)])=="YES"
  df_train$EVENT <-  as.character(df_train[, new_event_column_name] )=="YES"
    
  ##Obtain predictors for cox model.
  imp_rf <- importance(model_ttl$fit)
  predictor_rating<- imp_rf[imp_rf[, "%IncMSE"] > 0, "IncNodePurity"]
  predictor_rating <- sort(predictor_rating, decreasing = TRUE)
  topn = ceiling(length(predictor_rating) * .25) # only consider top n%
  
  flog.info("No of topN variables used for risk calculation : %s out of %s, out of full predictor %s", topn, length(predictor_rating), nrow(imp_rf))  
  
  formula = as.formula(paste("Surv(TIME, EVENT)" , paste(names(predictor_rating[c(1:topn)]),collapse="+"), sep=" ~ "))
  print(formula)
  library(survival)

  cox_fit = coxph(formula, df_train)
  print("Dim cox fit")
  print(cox_fit)
  
  #calc score
  risk_scores_test = predict(cox_fit,type="risk",se.fit=TRUE, newdata = df_test)
  risk_scores_train = predict(cox_fit,type="risk",se.fit=TRUE, newdata = df_train)
  
  
  #write output
  write.csv( risk_scores_test, file=file.path(outdir, paste("risk_scores_test",  time_period_in_days,".csv", sep="")))
  write.csv( risk_scores_train, file=file.path(outdir, paste("risk_scores_train", time_period_in_days ,".csv", sep="")))
  
  return(list(train=risk_scores_train, test =risk_scores_test ))
  #print(df_train[, c("risk_score", names(predictor_rating[c(1:topn)]))])
  
}

run_risk_score <- function(model_ttl, model_death, time_period_in_months, outdir){
  
  #apply time period on event
  df_train <- model_ttl$train
  df_train$EVENT <- ifelse(as.character(df_train$DEATH)=="YES", TRUE, FALSE)
  
  #apply time period on event for test  
  df_test <- model_ttl$test
  death_predictions <- model_death$predictions
  ttl_predictions <- model_ttl$predictions
  
  
#   df_test$DEATH <- death_predictions[rownames(df_test)]
#   df_test$LKADT_P <-  ttl_predictions[rownames(df_test)]
#   
#   df_test$EVENT <- apply(df_test, 1, function(x){
#     calc_event(time_period_in_months, 0)
#   })
  
  ##Obtain predictors for cox model.
  imp_rf <- importance(model_ttl$fit)
  predictor_rating<- imp_rf[imp_rf[, "%IncMSE"] > 0, "IncNodePurity"]
  predictor_rating <- sort(predictor_rating, decreasing = TRUE)
  topn = ceiling(length(predictor_rating) * .25) # only consider top n%
  
  flog.info("No of topN variables used for risk calculation : %s out of %s, out of full predictor %s", topn, length(predictor_rating), nrow(imp_rf))  
 
  formula = as.formula(paste("Surv(LKADT_P, EVENT)" , paste(names(predictor_rating[c(1:topn)]),collapse="+"), sep=" ~ "))
  flog.debug(formula)
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

predict_discontinuedflag <- function(subset_train, subset_test, dependent_variables, outdir){
  library("randomForest")
  flog.info("Begin function predict_discontinuedflag")
   #run RF, use rough fix for missing values
  
  subset_train.roughfix <- na.roughfix(subset_train)
  subset_test.roughfix <- na.roughfix(subset_test) 
   
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$DISCONT, ntree = 1000,  importance=TRUE)
  print("random forest fit: ")
  print(fit)
  
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
  flog.info("End function predict_discontinuedflag")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}


predict_discontinuedreason <- function(subset_train, subset_test, dependent_variables,  outdir){
  library("randomForest")
  library("futile.logger")
  flog.info("Begin function predict_discontinuedreason")
    
  #run RF, use rough fix for missing values
  
  subset_train.roughfix <- na.roughfix(subset_train)
  subset_test.roughfix <- na.roughfix(subset_test)
  
  flog.info("Running random forest")
  fit <- randomForest( subset_train.roughfix[, !colnames(subset_train.roughfix ) %in% dependent_variables] , y=subset_train.roughfix$ENDTRS_C, ntree = 1000,  mtry=30, importance=TRUE)
  print("Random forest fit")
  print(fit)
 
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