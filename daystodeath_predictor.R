source("./generic_s3_methods.R")

# Discontinued reason model
daystodeath_predictor <- function(challenge_data_train, challenge_data_test,  out_dir, seed_files=NULL, rseeds_out_dir="./random_seeds", mtry=15, ntree=150){
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
              out_dir=setup_outdir(out_dir, "daystodeath_predictor"),
              fit=NULL,
              x_predictions=NULL,
              new_data_predictions=NULL,
              x_predictions_score=NULL,
              new_data_predictions_score=NULL)
  class(out) <- "daystodeath_predictor"
  
  
  ### Remove irrelavant  data, data with discontinued is > discont_in_months 
  
  
  
  invisible(out)
}


#This is the processing pipeline
run_pipeline.daystodeath_predictor <- function(object){
  flog.info("Begin run_pipeline.daystodeath_predictor")
  
  #cleanup 
  object <- cleanup(object)
  #model fit
  object <- model(object)
  #write all to file
  write_to_file(object)
  #return results
  object <- score_model(object)
  summary(object)
  
  flog.info("End run_pipeline.daystodeath_predictor")
  invisible(object)
  
}


#This cleans up data, most boring but complicated
cleanup.daystodeath_predictor <- function(object){
  flog.info("Begin cleanup.daystodeath_predictor")  
  
  #regular data cleaning
  challenge_data_train <- cleanup(object$challenge_data_train)
  challenge_data_test <- cleanup(object$challenge_data_test)
  ycols <- challenge_data_train$ycols
  
  #only train with data with relavant samples, death =="yes" 
  rowsnames_of_relavant_data <- rownames(ycols[(as.character(ycols$DEATH) ==  "YES"), ])
  flog.info("Extracting %s out of total out of %s train records", length(rowsnames_of_relavant_data), nrow(ycols))
  challenge_data_train$ct <- challenge_data_train$ct[rowsnames_of_relavant_data, ]
  
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
  
  y <- ycols[rownames(object$x), "LKADT_P"]
  names(y) <- rownames(object$x)
  object$y <-  y
  
  object$new_data <- aligned_train_test$df_test
  
  #cleanup train  
  flog.info("End cleanup.daystodeath_predictor")  
  
  return(object)
  
}




#This models the data using randomforest
model.daystodeath_predictor <- function(object){
  flog.info("Begin model.daystodeath_predictor")
  library("randomForest")
  
  if (is.null(object$x) | is.null(object$y)){
    stop("Please run cleanup before model is called")
  }
  x <- na.roughfix(object$x)
  y <- object$y
  new_data <- na.roughfix(object$new_data)
  
  ##Average Random forest runs over n iterations
  n_runs = 5
  if ( !is.null(object$seed_files)){
    n_runs = length(object$seed_files)
  }
  
  
  x_pred_total <- init_named_vector_from_data_frame(x, 0)
  new_data_pred_total <- init_named_vector_from_data_frame(new_data, 0)
  
  for( i in c(1:n_runs)){
    #### House keeping
    #set up seed file
    if ( !is.null(object$seed_files)){
      restore_rng(object$seed_files[i])
    }else{
      set.seed(NULL)
      save_rng(file.path(object$rseeds_out_dir, paste("model_daystodeath_predictor_", i, ".seed", sep="")))
    }
    
    ####model using randomforest
    fit <- randomForest(x, y, mtry=object$mtry, ntree=object$ntree, importance=TRUE)
    print(fit)
    
    x_pred_total <- x_pred_total + fit$predicted[names(x_pred_total)]
    new_data_pred_total <- new_data_pred_total + predict(fit,  new_data )[names(new_data_pred_total)]
  }
  
  ####output
  object$fit <- fit
  object$x_predictions <- x_pred_total/n_runs
  object$new_data_predictions <- new_data_pred_total/n_runs
  
  flog.info("End model.daystodeath_predictor")
  return(object)
}

score_model.daystodeath_predictor <- function(object){
  if (is.null(object$x_predictions) ){
    stop("Please invoke model before score is called")
  }
  source("./score.R")
  object$x_predictions_score <- score_q1b(object$x_predictions[names(object$y)], object$y, object$challenge_data_train$ycols[names(object$y), "DEATH"])
  
  #run test score if running in test mode
  if (!is.null(object$challenge_data_test$ycols)){
    actual <- object$challenge_data_test$ycols$LKADT_P
    names(actual) <- rownames(object$challenge_data_test$ycols)  
    object$new_data_predictions_score <- score_q1b(object$new_data_predictions[names(actual)], actual, object$challenge_data_test$ycols[names(actual), "DEATH"])    
  }
  
  return(object)
}

#This writes all data into file
write_to_file.daystodeath_predictor <- function(object){
  #if data is clean, write x & y
  if (!is.null(object$x)){
    write.csv(merge_data_frame_with_named_vector(merge_data_frame_by_rows(object$x, object$challenge_data_train$ycols), object$y), file=file.path(object$out_dir, "inputdata_x.csv"))
  }
  
  #if model is available, save importance measure
  if (!is.null(object$fit)){
    write.csv( importance(object$fit) , file=file.path(object$out_dir, "importanceFit.csv")) 
  }
  
  #if x_predictions are is available, save actual & predicted
  if (!is.null(object$x_predictions)){
    write.csv( merge_data_frame_with_named_vector(merge_data_frame_by_rows(object$x, object$challenge_data_train$ycols), object$x_predictions, suffixes = c(".actual", ".pred") ), 
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
  }
  write.csv(data.frame(RPT=names(object$new_data_predictions) , TIMETOEVENT=object$new_data_predictions), file=file.path(object$out_dir, "submission1b.csv"), row.names=FALSE)
  
  
}


print.daystodeath_predictor <- function(object){
  #todo
  print("x data structure used")
  print(str(object$x))
  print("y values struct in train")
  print(str(object$y))
  print("new data struct")
  print(str(object$new_data))
  cat("Discontinued in ", object$discont_in_days)
  print("Model:")
  
  print(object$fit)
  
  
  
}

summary.daystodeath_predictor <- function(object){
  #todo
  print("dim x")
  print(dim(object$x))
  print("length y")
  print(length(object$y))
  print("dim new_data")
  print(dim(object$new_data))
  print(paste("Discontinued in ", object$discont_in_days, sep=""))
  #   print("summary train")
  #   summary(object$challenge_data_train)
  #   print("summary test")
  #   summary(object$challenge_data_test)
  print("summary fit")
  print(object$fit)
  
  print("seed files used")
  print(object$seed_files)
  print("scores for x predictions")
  print(object$x_predictions_score)
  print("scores for new data predictions")
  print(object$new_data_predictions_score)
}


predict.daystodeath_predictor <- function(object, newdata){
  return( predict(object$fit, na.roughfix(object$new_data) ))
}