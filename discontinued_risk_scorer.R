source("./generic_s3_methods.R")





# Discontinued reason model
discontinued_risk_scorer <- function(challenge_data_train, challenge_data_test, discont_in_days, out_dir, seed_files=NULL,rseeds_out_dir="./random_seeds", mtry=25, ntree=1000){
  #######todo validate
  #x must contain ENTRT_PC column
  
  ####output
  out <- list(challenge_data_train = challenge_data_train, 
              challenge_data_test = challenge_data_test,
              seed_files = seed_files,
              mtry=mtry,
              ntree=ntree,
              rseeds_out_dir=rseeds_out_dir,
              x=NULL,
              y=NULL,  
              new_data = NULL,
              out_dir=setup_outdir(out_dir, "discontinued_risk_scorer"),
              discont_in_days=discont_in_days, 
              fit=NULL,
              x_predictions=NULL,
              new_data_predictions=NULL,
              x_predictions_score=NULL,
              new_data_predictions_score=NULL)
  class(out) <- "discontinued_risk_scorer"
  
  
  ### Remove irrelavant  data, data with discontinued is > discont_in_months 
  
  
  
  invisible(out)
}


#This is the processing pipeline
run_pipeline.discontinued_risk_scorer <- function(object){
  flog.info("Begin run_pipeline.discontinued_risk_scorer")
  
  #cleanup 
  object <- cleanup(object)
  #model fit
  object <- model(object)
  #write all to file
  write_to_file(object)
  #return results
  object <- score_model(object)
  summary(object)
  
  flog.info("End run_pipeline.discontinued_risk_scorer")
  invisible(object)
  
}


#This cleans up data, most boring but complicated
cleanup.discontinued_risk_scorer <- function(object){
  flog.info("Begin cleanup.discontinued_risk_scorer")  
  
  #regular data cleaning
  object$challenge_data_train <- cleanup(object$challenge_data_train)
  object$challenge_data_test <- cleanup(object$challenge_data_test)
  challenge_data_train <- object$challenge_data_train
  challenge_data_test <- object$challenge_data_test
  ycols <- challenge_data_train$ycols
    
  
  #remove data that is not used before merge, nullify data sets that r not used
  get_relevant_dataset <- function(challenge_data){  
    challenge_data$lm <- data.frame()
   # challenge_data$lv <- data.frame()
    challenge_data$mh <- data.frame()
    challenge_data$vs <- data.frame()
    #    challenge_data$pm <- data.frame()
    return(challenge_data)
  }
  
  
  
  #remove values with DISCONT blank
  challenge_data_train$ct <- challenge_data_train$ct[rownames(challenge_data_train$ycols)[as.character(challenge_data_train$ycols$DISCONT) =="-1"], ]
  
  
  #discontinued is undersamppled.. so remove sampled randomly 
  n_row_names_of_discontinued <- rownames(challenge_data_train$ycols)[as.character(challenge_data_train$ycols$DISCONT) == "1"]
  df_not_discontinued <- challenge_data_train$ct[ ! rownames(challenge_data_train$ct) %in% n_row_names_of_discontinued, ]
  get_random_seed(object, seed_file_index.discontinued_risk_scorer()$sampler) 
  df_not_discontinued <- df_not_discontinued[sample(nrow(df_not_discontinued), length(n_row_names_of_discontinued)*2), ]
  challenge_data_train$ct <- rbind(df_not_discontinued, challenge_data_train$ct[n_row_names_of_discontinued, ])
  
  
  #merge parts of data into one df
  challenge_data_train <- merge(get_relevant_dataset(challenge_data_train))
  challenge_data_test <- merge(get_relevant_dataset(challenge_data_test))
  summary(challenge_data_train)  
  summary(challenge_data_test)  
  
  aligned_train_test <- align_train_test(challenge_data_train$merged_data, challenge_data_test$merged_data)
  
  
  ##assign the final, x, y and test new data after cleanup.
  object$x <-  aligned_train_test$df_train  
  
  y <- ycols[rownames(object$x), c("DISCONT", "ENDTRS_C", "ENTRT_PC")]
  y$EVENT <- apply(y, 1, function(a){calc_event.discontinued_risk_scorer(a, object$discont_in_days)})
  object$y <-  y
  
  object$new_data <- aligned_train_test$df_test
  object$challenge_data_train <- challenge_data_train
  object$challenge_data_test <- challenge_data_test
  
  #cleanup train  
  flog.info("End cleanup.discontinued_risk_scorer")  
  
  return(object)
  
}


write_cv_lassocox_fit.survivalanalysis_risk_scorer <- function(fit, out_dir){
  write.csv(data.frame(lambda=fit$lambda, cvl=fit$cvl), file=file.path(out_dir, "profl1.csv"))
  pdf(file.path( out_dir, "plots_run_risk_score_lassocox_profL1.pdf" ))
  plot(fit$lambda, fit$cvl,  type="l", log="x")
  dev.off()
}

#This models the data using randomforest
model.discontinued_risk_scorer <- function(object){
  flog.info("Begin model.discontinued_risk_scorer")
  library("randomForest")
  library("penalized")
  
  if (is.null(object$x) | is.null(object$y)){
    stop("Please run cleanup before model is called")
  }
  
  x <- na.roughfix(object$x)
  y <- object$y
  new_data <-na.roughfix(object$new_data)
  
  
  
  #### House keeping
  get_random_seed(object, seed_file_index.discontinued_risk_scorer()$randomforest)
  ####step 1: model using randomforest
  
  rfit <- randomForest(x, as.factor(y[rownames(x), "EVENT"]), mtry=object$mtry, ntree=object$ntree, importance=TRUE)
  print("Random forest fit")
  print(rfit)
  
  ###step 2: get top important variables
  get_important_variables <- function(fit, topnpercent = 1.0){
    imp_rf <- importance(fit)
    predictor_rating<- imp_rf[imp_rf[,"MeanDecreaseAccuracy"] > 0, "MeanDecreaseGini"]
    predictor_rating <- sort(predictor_rating, decreasing = TRUE)
    topn = ceiling(length(predictor_rating) * topnpercent)
    return(names(predictor_rating[c(1:topn)]))
  }
  important_variables <-get_important_variables(rfit, 1.0)
  
  ##step 3:run lasso cox using the important variables
  formula = as.formula(paste("~", paste(important_variables,collapse="+"), sep=" "))
  print(formula)
  
  # Step 3a - Run profiler to get optimal lamba to avoid local minima   
  x$EVENT <-  y[rownames(x), "DISCONT"]
  x$ENTRT_PC <- y[rownames(x), "ENTRT_PC"]

  write.csv(data.frame(event = x$EVENT, time=  x$ENTRT_PC), file=file.path(object$out_dir, "event vs time.csv"))
  plot_kaplan_meier_survival( x$ENTRT_PC,x$EVENT, file.path(object$out_dir, "kepmlen_mier_risk_plot.pdf"))
  profiler_fit <- profL1(Surv(ENTRT_PC, EVENT), penalized=formula,   data = x,
                         model = c("cox"), fold=10 , minlambda1=.01, maxlambda1= 7000, trace=FALSE, plot=FALSE)  
  write_cv_lassocox_fit.survivalanalysis_risk_scorer(profiler_fit, object$out_dir)
  
  #Step 3b - Using the range  (obtained manually inspecting the graph) in the profiler run lasscox to obtain optimum lambda
  get_random_seed(object, seed_file_index.discontinued_risk_scorer()$coxcvoptima)
  cv <- optL1 (Surv(ENTRT_PC, EVENT), penalized=formula, data = x,
               model = c("cox"), trace=FALSE, minlambda1=3000, maxlambda1=3500)
  print(paste ("Cross validation lambda for cox is", cv$lambda , sep=" ") )
  
  #Step 3c - Using the optimum lambda , run lassocox  
  cox_fit <- penalized(Surv(ENTRT_PC, EVENT), penalized = formula,
                       data = x, model=c("cox"), lambda1=cv$lambda)
  
 
  ####output
  object$fit <- cox_fit
  object$x_predictions <- 1/(survival(predict(cox_fit, data = x), 24 * 30.5))

  object$new_data_predictions <- 1/(survival(predict(cox_fit, data = new_data), 24 * 30.5))
  
  flog.info("End model.discontinued_risk_scorer")
  return(object)
}

score_model.discontinued_risk_scorer <- function(object){
  if (is.null(object$x_predictions) ){
    stop("Please invoke model before score is called")
  }
  source("./score.R")
  object$x_predictions_score <- score_q2(object$x_predictions, object$y[names(object$x_predictions), "EVENT"])
  
  #run test score if running in test mode
  if (!is.null(object$challenge_data_test$ycols)){
    
    actual <- object$challenge_data_test$ycols$DISCONT #apply(object$challenge_data_test$ycols, 1, function(a){calc_event.discontinued_risk_scorer(a, object$discont_in_days)})    
    names(actual) <- rownames(object$challenge_data_test$ycols)  
    
    object$new_data_predictions_score <- score_q2(object$new_data_predictions, actual[names(object$new_data_predictions)])    
  }
  
  return(object)
}

#This writes all data into file
write_to_file.discontinued_risk_scorer <- function(object){
  #if data is clean, write x & y
  if (!is.null(object$x)){
    write.csv(merge_data_frame_by_rows(merge_data_frame_by_rows(object$x, object$challenge_data_train$ycols), object$y), file=file.path(object$out_dir, "inputdata_x.csv"))
  }
  
  #if x_predictions are is available, save actual & predicted
  if (!is.null(object$x_predictions)){
    write.csv( merge_data_frame_with_named_vector(merge_data_frame_by_rows(object$x, object$y), object$x_predictions, suffixes = c(".actual", ".pred") ), 
               file=file.path(object$out_dir, "inputdata_x_predictions.csv")) 
  }
  
  #write predictions
  if (!is.null(object$new_data_predictions)){
    ##special case if running in train mode, merge predicted new data with actual
    data <- object$new_data
    if (! is.null(object$challenge_data_test$ycols)){
      data <- merge_data_frame_by_rows(data, object$challenge_data_test$ycols)
    }
    write.csv( merge_data_frame_with_named_vector(data, object$new_data_predictions, suffixes = c(".actual", ".pred") ), 
               file=file.path(object$out_dir, "new_data_predictions.csv")) 
    
    write.csv( RPT=rownames(object$new_data_predictions), RISK=object$new_data_predictions$RISK_SCORE, DISCONT=ifelse(as.character(object$new_data_predictions$DISCONT)=="Zero", 0, 1), 
    file=file.path(object$out_dir, "submission2.csv")) 
  }
  
}

seed_file_index.discontinued_risk_scorer <- function(){
  list(randomforest=1, coxcvoptima=2, sampler = 3)
}

get_random_seed.discontinued_risk_scorer <- function(object, index){
  flog.info("Begin get_random_seed.discontinued_risk_scorer")
  restore = FALSE;
  if (!is.null(object$seed_files)){
    if (!is.na(object$seed_files[index])){
      restore = TRUE
    }
  }
  if (restore){
    flog.info("Restoring seed file %s", object$seed_files[index])
    restore_rng(object$seed_files[index])
  }
  else{
    flog.info("Using new seed file for index %s", index)
    set.seed(NULL)
    seed_file_prefix <-  as.character(sys.calls()[[sys.nframe()-2]])[1]
    print(seed_file_prefix)
    save_rng(file.path(object$out_dir, paste(seed_file_prefix,"_",index,".seed", sep="")))
    save_rng(file.path(object$rseeds_out_dir, paste(seed_file_prefix,"_",index,".seed", sep="")))
    
  }
  flog.info("End get_random_seed.discontinued_risk_scorer")
}

print.discontinued_risk_scorer <- function(object){
  #todo
  print("x data structure used")
  print(str(object$x))
  print("y values struct in train")
  print(str(object$y))
  print("new data struct")
  print(str(object$new_data))
  cat("Discontinued in ", object$discont_in_days)
  print("Model:")
  print(object$model)
  
}

summary.discontinued_risk_scorer <- function(object){
  #todo
  print("dim x")
  print(dim(object$x))
  print("length y")
  print(dim(object$y))
  print("dim new_data")
  print(dim(object$new_data))
  print(paste("Discontinued in ", object$discont_in_days, sep=""))
  #   print("summary train")
  #   summary(object$challenge_data_train)
  #   print("summary test")
  #   summary(object$challenge_data_test)
  print("summary fit")
  print(object$fit)
  print(show(object$fit))
  print(coefficients (object$fit))
  print("scores for x predictions")
  print(object$x_predictions_score)
  print("scores for new data predictions")
  print(object$new_data_predictions_score)
 
}


