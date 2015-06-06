library(futile.logger)

predict_timetolive <- function(subset_train, subset_test, dependent_variables, outdir){
  library("randomForest")
  flog.info("Begin function predict_timetolive")
    
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
  flog.info("End function predict_timetolive")
  return(list(predictions=predictions, fit=fit, train=subset_train.roughfix, test=subset_test.roughfix ))
  
}

predict_death <- function(subset_train, subset_test, dependent_variables, outdir){
  
  flog.info("Begin function predict_death")
  library("randomForest")
  
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
  flog.info("No of topN variables used for risk calculation : %s out of %s", topn, length(predictor_rating))  
 
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