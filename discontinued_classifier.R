source("./generic_s3_methods.R")

# Discontinued reason model
discontinued_classifier <- function(challenge_data_train, challenge_data_test, discont_in_days, out_dir, seed_file="", mtry=15, ntree=250){
  #######todo validate
  #x must contain ENTRT_PC column
  
  ####output
  out <- list(challenge_data_train = challenge_data_train, 
              challenge_data_test = challenge_data_test,
              seed_file = seed_file,
              mtry=mtry,
              ntree=ntree,
              x=NULL,
              y=NULL,  
              new_data = NULL,
              out_dir=setup_outdir(out_dir, "discontinued_classifier"),
              discont_in_days=discont_in_days, 
              fit=NULL,
              x_predictions=NULL,
              new_data_predictions=NULL,
              x_predictions_score=NULL,
              new_data_predictions_score=NULL)
  class(out) <- "discontinued_classifier"
  
  
  ### Remove irrelavant  data, data with discontinued is > discont_in_months 
  
  
  
  invisible(out)
}


#This is the processing pipeline
run_pipeline.discontinued_classifier <- function(object){
  flog.info("Begin run_pipeline.discontinued_classifier")
  
  #cleanup 
  object <- cleanup(object)
  #model fit
  object <- model(object)
  #write all to file
  write_to_file(object)
  #return results
  object <- score_model(object)
  summary(object)
  
  flog.info("End run_pipeline.discontinued_classifier")
  invisible(object)
  
}


#This cleans up data, most boring but complicated
cleanup.discontinued_classifier <- function(object){
  flog.info("Begin cleanup.discontinued_classifier")  
  
  #regular data cleaning
  challenge_data_train <- cleanup(object$challenge_data_train)
  challenge_data_test <- cleanup(object$challenge_data_test)
  ycols <- challenge_data_train$ycols
  
  #only train with data with relavant samples, ENDTRS_C < discont_in_days 
  #   rows_nos_with_releavant_data <- which(ycols$ENDTRS_C <= object$discont_in_days)
  #   rownames_of_relavent_data <- rownames(ycols)[rows_nos_with_releavant_data]   
  #   challenge_data_train$ct <- challenge_data_train$ct[rownames_of_relavent_data, ]
  #   challenge_data_train$ycols <-challenge_data_train$ycols[rownames_of_relavent_data]
  #     
  
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
  
  y <- ycols[rownames(object$x), "DISCONT"]
  names(y) <- rownames(object$x)
  object$y <-  y
  
  object$new_data <- aligned_train_test$df_test
  
  #cleanup train  
  flog.info("End cleanup.discontinued_classifier")  
  
  return(object)
  
}




#This models the data using randomforest
model.discontinued_classifier <- function(object){
  flog.info("Begin model.discontinued_classifier")
  library("randomForest")
  
  if (is.null(object$x) | is.null(object$y)){
    stop("Please run cleanup before model is called")
  }
  x <- object$x
  y <- object$y
  
  #### House keeping
  #set up seed file
  if (object$seed_file != ""){
    restore_rng(object$seed_file)
  }else{
    set.seed(NULL)
    save_rng(file.path(object$out_dir, paste( "discontinued_classifier_model_", format(Sys.time(), "%Y%m%d_%H%M%S"),".seed", sep="")))
  }
  
  ####model using randomforest
  fit <- randomForest(na.roughfix(x), y, mtry=object$mtry, ntree=object$ntree, importance=TRUE)
  print("Random forest fit")
  print(fit)
  
  ####output
  object$fit <- fit
  object$x_predictions <- fit$predicted
  object$new_data_predictions <- predict(object$fit, na.roughfix(object$new_data) )
  
  flog.info("End model.discontinued_classifier")
  return(object)
}

score_model.discontinued_classifier <- function(object){
  if (is.null(object$x_predictions) ){
    stop("Please invoke model before score is called")
  }
  source("./score.R")
  object$x_predictions_score <- score_classification(object$y, object$x_predictions[names(object$y)])
  
  #run test score if running in test mode
  if (!is.null(object$challenge_data_test$ycols)){
    actual <- object$challenge_data_test$ycols$DISCONT
    names(actual) <- rownames(object$challenge_data_test$ycols)  
    object$new_data_predictions_score <- score_classification(actual, object$new_data_predictions[names(actual)])    
  }
  
  return(object)
}

#This writes all data into file
write_to_file.discontinued_classifier <- function(object){
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
    write.csv( merge_data_frame_with_named_vector(merge_data_frame_with_named_vector(object$x, object$y), object$x_predictions, suffixes = c(".actual", ".pred") ), 
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
    
}


print.discontinued_classifier <- function(object){
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

summary.discontinued_classifier <- function(object){
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
  print("scores for x predictions")
  print(object$x_predictions_score)
  print("scores for new data predictions")
  print(object$new_data_predictions_score)
}


predict.discontinued_classifier <- function(object, newdata){
  return( predict(object$fit, na.roughfix(object$new_data) ))
}