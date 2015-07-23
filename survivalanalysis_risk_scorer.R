source("./generic_s3_methods.R")

# survivalanalysis risk scorer 
survivalanalysis_risk_scorer <- function(challenge_data_train, challenge_data_test,  out_dir, seed_files=NULL, rseeds_out_dir="./random_seeds", mtry=15, ntree=250){
  #######todo validate
  #x must contain ENTRT_PC column
  
  ####output
  out <- list(challenge_data_train = challenge_data_train, 
              challenge_data_test = challenge_data_test,
              seed_files = seed_files,
              mtry=mtry,
              ntree=ntree,
              rseeds_out_dir = rseeds_out_dir,
              x=NULL,
              y=NULL,  
              new_data = NULL,
              out_dir=setup_outdir(out_dir, "survivalanalysis_risk_scorer"),
              fit=NULL,
              x_predictions=NULL,
              new_data_predictions=NULL,
              x_predictions_score=NULL,
              new_data_predictions_score=NULL)
  class(out) <- "survivalanalysis_risk_scorer"
  
  
  ### Remove irrelavant  data, data with discontinued is > discont_in_months 
  
  
  
  invisible(out)
}


#This is the processing pipeline
run_pipeline.survivalanalysis_risk_scorer <- function(object){
  flog.info("Begin run_pipeline.survivalanalysis_risk_scorer")
  
  #cleanup 
  object <- cleanup(object)
  #model fit
  object <- model(object)
  #write all to file
  write_to_file(object)
  #return results
  object <- score_model(object)
  summary(object)
  
  flog.info("End run_pipeline.survivalanalysis_risk_scorer")
  invisible(object)
  
}


#This cleans up data, most boring but complicated
cleanup.survivalanalysis_risk_scorer <- function(object){
  flog.info("Begin cleanup.survivalanalysis_risk_scorer")  
  
  #regular data cleaning
  challenge_data_train <- cleanup(object$challenge_data_train)
  challenge_data_test <- cleanup(object$challenge_data_test)
  ycols <- challenge_data_train$ycols
   
  #remove data that is not used before merge, nullify data sets that r not used
  get_relevant_dataset <- function(challenge_data){  
    challenge_data$lm <- data.frame()
    challenge_data$lv <- data.frame()
    challenge_data$mh <- data.frame()
    challenge_data$vs <- data.frame()
    challenge_data$pm <- data.frame()
    return(challenge_data)
  }
  
  #merge parts of data into one df
  challenge_data_train <- merge(get_relevant_dataset(challenge_data_train))
  challenge_data_test <- merge(get_relevant_dataset(challenge_data_test))
  
  aligned_train_test <- align_train_test(challenge_data_train$merged_data, challenge_data_test$merged_data)
  
  ##assign the final, x, y and test new data after cleanup.
  object$x <-  aligned_train_test$df_train  
  
  y <- ycols[rownames(object$x), c("LKADT_P", "DEATH")]
  object$y <-  y
  
  object$new_data <- aligned_train_test$df_test
  
  #cleanup train  
  flog.info("End cleanup.survivalanalysis_risk_scorer")  
  
  return(object)
  
}

write_cv_lassocox_fit.survivalanalysis_risk_scorer <- function(fit, out_dir){
  write.csv(data.frame(lambda=fit$lambda, cvl=fit$cvl), file=file.path(out_dir, "profl1.csv"))
  pdf(file.path( out_dir, "plots_run_risk_score_lassocox_profL1.pdf" ))
  plot(fit$lambda, fit$cvl,  type="l", log="x")
  dev.off()
}


#This models the data using randomforest
model.survivalanalysis_risk_scorer <- function(object){
  flog.info("Begin model.survivalanalysis_risk_scorer")
  library("randomForest")
  library("penalized")
  ##seedfiles index
  s_randforest_index = 1
  s_lasso_index = 2
  
  if (is.null(object$x) | is.null(object$y)){
    stop("Please run cleanup before model is called")
  }
  x <- object$x 
  y <- object$y
  
  #### House keeping
  #set up seed file
  set_seed <- function(i){
    if (!is.null(object$seed_files)){
      restore_rng(object$seed_files[i])
    }else{
      set.seed(NULL)
      save_rng(file.path(object$rseeds_out_dir, paste("model_survivalanalysis_risk_scorer_", i, ".seed", sep="")))
    }
  }
  
  set_seed(s_randforest_index)
  ####Setp 1: Use randomforest to obtain importance measure
  x <- na.roughfix(x)
  rfit <- randomForest(x, y$LKADT_P, mtry=object$mtry, ntree=object$ntree, importance=TRUE)
  print("Random forest fit")
  print(rfit)
  
  ###Setup 2: Get most important variables from the fit
  get_important_variables <- function(fit, topnpercent = 1.0){
    imp_rf <- importance(fit)
    predictor_rating<- imp_rf[imp_rf[, "%IncMSE"] > 0, "IncNodePurity"]
    predictor_rating <- sort(predictor_rating, decreasing = TRUE)
    topn = ceiling(length(predictor_rating) * topnpercent)
    return(names(predictor_rating[c(1:topn)]))
  }
  important_variables <- get_important_variables(rfit)
  
  ###Step 3: Run lasso cox using the important variables
  formula = as.formula(paste("~", paste(important_variables,collapse="+"), sep=" "))
  print(formula)
  
  # Step 3a - Run profiler to get optimal lamba to avoid local minima   
  x$EVENT <-  ifelse(as.character(object$y[rownames(x), "DEATH"]) == "YES", 1, 0)
  x$LKADT_P <- object$y[rownames(x), "LKADT_P"]

  profiler_fit <- profL1(Surv(LKADT_P, EVENT), penalized=formula,   data = x,
                 model = c("cox"), fold=10 , minlambda1=0.01, maxlambda1= 10000, trace=FALSE, plot=FALSE)  
  write_cv_lassocox_fit.survivalanalysis_risk_scorer(profiler_fit, object$out_dir)
  
  #Step 3b - Using the range  (obtained manually inspecting the graph) in the profiler run lasscox to obtain optimum lambda
  set_seed(s_lasso_index)
  cv <- optL1 (Surv(LKADT_P, EVENT), penalized=formula, data = x,
               model = c("cox"), trace=FALSE, minlambda1=1, maxlambda1=500)
  print(paste ("Cross validation lambda for cox is", cv$lambda , sep=" ") )
  
  #Step 3c - Using the optimum lambda , run lassocox  
  cox_fit <- penalized(Surv(LKADT_P, EVENT), penalized = formula,
                       data = x, model=c("cox"), lambda1=cv$lambda)
  
  ####output
  object$fit <- cox_fit
  object$x_predictions <- 1/(survival(predict(cox_fit,formula, data = na.roughfix(object$x)), 12*30))
  object$new_data_predictions <- 1/(survival(predict(cox_fit,formula, data = na.roughfix(object$new_data)), 12*30))
 
  flog.info("End model.survivalanalysis_risk_scorer")
  return(object)
}

score_model.survivalanalysis_risk_scorer <- function(object){
  if (is.null(object$x_predictions) ){
    stop("Please invoke model before score is called")
  }
  source("./score.R")
  object$x_predictions_score <- score_q1a(object$y$LKADT_P , object$y$DEATH, object$x_predictions[rownames(object$y)])
  
  #run test score if running in test mode
  if (!is.null(object$challenge_data_test$ycols)){
    actual <- object$challenge_data_test$ycols        
    object$new_data_predictions_score <- score_q1a(actual$LKADT_P, actual$DEATH, object$new_data_predictions[rownames(actual)])    
  }
  
  return(object)
}

#This writes all data into file
write_to_file.survivalanalysis_risk_scorer <- function(object){
  #if data is clean, write x & y
  if (!is.null(object$x)){
    write.csv(merge_data_frame_by_rows(merge_data_frame_by_rows(object$x, object$challenge_data_train$ycols), object$y), file=file.path(object$out_dir, "inputdata_x.csv"))
  }
  
  
  #if x_predictions are is available, save actual & predicted
  if (!is.null(object$x_predictions)){
    write.csv( merge_data_frame_with_named_vector(merge_data_frame_by_rows(object$x, object$challenge_data_train$ycols), object$x_predictions, suffixes = c(".actual", ".pred") ), 
               file=file.path(object$out_dir, "inputdata_x_riskscore.csv")) 
  }
  
  #write predictions
  if (!is.null(object$new_data_predictions)){
    ##special case if running in train mode, merge predicted new data with actual
    data <- object$new_data
    if (! is.null(object$challenge_data_test$ycols)){
      data <- merge_data_frame_by_rows(data, object$challenge_data_test$ycols)
    }
    write.csv( merge_data_frame_with_named_vector(data, object$new_data_predictions, suffixes = c(".actual", ".pred") ), 
               file=file.path(object$out_dir, "new_data_risk_scores.csv")) 
  }
  write.csv(data.frame(RPT=names(object$new_data_predictions) , riskScoreGlobal=object$new_data_predictions), file=file.path(object$out_dir, "submission1a.csv"), row.names=FALSE)
}


print.survivalanalysis_risk_scorer <- function(object){
  #todo
  print("x data structure used")
  print(str(object$x))
  print("y values struct in train")
  print(str(object$y))
  print("new data struct")
  print(str(object$new_data))
  print("Seed file used")
  print(object$seed_files)
  print("Model:")
  print(object$model)
  
}

summary.survivalanalysis_risk_scorer <- function(object){
  #todo
  print("dim x")
  print(dim(object$x))
  print("length y")
  print(length(object$y))
  print("dim new_data")
  print(dim(object$new_data))
  print("Seed file used")
  print(object$seed_files)
  #   print("summary train")
  #   summary(object$challenge_data_train)
  #   print("summary test")
  #   summary(object$challenge_data_test)
  print("summary fit")
  print(object$fit)
  print("scores for x predictions")
  print(object$x_predictions_score)
  print("scores for new data predictions")
  print(object$new_data_predictions_score)
}


predict.survivalanalysis_risk_scorer <- function(object, newdata){
  return( predict(object$fit, na.roughfix(object$new_data) ))
}