#this methods aligns test data structure with test
align_train_test <- function(df_train, df_test){  
  library(futile.logger)
  
  subset_train <- df_train  
  subset_test <- df_test
  
  #remove columns with all NA
  subset_train <-remove_all_nan_columns(remove_all_na_columns(subset_train))
  subset_test <- remove_all_nan_columns(remove_all_na_columns(subset_test))
  
  #retain only columns common to both test and train
  commonCols <- Reduce(intersect, list(colnames(subset_train), colnames(subset_test))) 
  subset_train <- subset_train[, commonCols]
  subset_test <- subset_test[, commonCols]
  
  datasets_aligned_factors <-  make_factors_alike(subset_train, subset_test)
  
  return(list(df_train = datasets_aligned_factors$train, df_test=datasets_aligned_factors$test))
  
}

make_factors_alike <- function(subset_train, subset_test){
  library(futile.logger)
  commonCols = Reduce(intersect, list(colnames(subset_train), colnames(subset_test)))
  columns_to_remove =c()
  for(c in commonCols){
    if(is.double(subset_train[, c(c)]) & is.integer(subset_test[, c(c)])){
      subset_test[, c(c)] = as.double(subset_test[, c(c)])
      message <- paste ("The type of", c, "train", typeof(subset_train[, c(c)]) , "does not match the type in test", typeof(subset_test[, c(c)]), ". hence converting int to double")
      flog.debug(message)
    }
    if(is.integer(subset_train[, c(c)]) & is.double(subset_test[, c(c)])){
      subset_train[, c(c)] = as.double(subset_train[, c(c)])
      message <- paste ("The type of", c, "train", typeof(subset_train[, c(c)]) , "does not match the type in test", typeof(subset_test[, c(c)]), ". hence converting int to double")
      flog.debug(message)
    }
    
    if(is.factor(subset_train[, c(c)]) & is.atomic(subset_test[, c(c)])){
      subset_test[, c(c)] = as.factor(subset_test[, c(c)])
      message <- paste ("The type of", c, "train", typeof(subset_train[, c(c)]) , "does not match the type in test", typeof(subset_test[, c(c)]), ". hence converting int to double")
      flog.debug(message)
    }
    
    if (typeof(subset_train[, c(c)]) != typeof(subset_test[, c(c)])){
      message <- paste ("The type of", c, "train", typeof(subset_train[, c(c)]) , "does not match the type in test", typeof(subset_test[, c(c)]))
      flog.warn(message)
      columns_to_remove = c(columns_to_remove, c)
      warning (message)   
      next
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
    
  }#end of loop for each column
  if (length(columns_to_remove) >0 ){
    flog.warn("Removing columns due to type mismatch %s", columns_to_remove)
    subset_test <- subset_test[, !colnames(subset_test) %in% columns_to_remove]
    subset_train <- subset_train[, !colnames(subset_train) %in% columns_to_remove]
  }
  
  return(list(train = subset_train, test = subset_test))
}

remove_all_na_columns <- function(mydataframe){
  
  bad <- sapply(mydataframe, function(x) {    
    all(is.na(x))
  })
  
  flog.info("Removing columns with all NA %s",  colnames(mydataframe[,bad]))
  return( mydataframe[,!bad])
}

remove_all_nan_columns <- function(mydataframe){
  bad <- sapply(mydataframe, function(x) {all(is.nan(x))})
  flog.info("Removing columns with all NAN %s",  colnames(mydataframe[,bad]))
  
  return( mydataframe[,!bad])
}



convert_to_yes_no_factor <- function(dataset_df, col_name){
  dataset_df[, c(col_name)] <- as.factor(dataset_df[, c(col_name)] )
  levels(dataset_df[, c(col_name)] ) <- c("YES", "NO")
  return (dataset_df)
}


clean_names <- function(cols){
  result <- gsub("-|\\s+|#|\\(|\\)|\\/|,|'","_", cols)
  result <- gsub("_+","_", result)  
  result <- gsub("^_","", result)  
  result <- gsub("_$","", result)  
  return (result)
}

remove_duplicated_record <- function(data, cols){
  library(futile.logger)
  #remove duplicated lab results
  duplicated_data <- duplicated(data[,  cols])
  duplicate_count <-length (duplicated_data[duplicated_data == TRUE])
  
  if ( duplicate_count > 0){
    warning(paste("Duplicates found:", duplicate_count,  "These will be removed!!", sep = " " ))
    flog.warn("Duplicate lab result record keys found")
    flog.debug(data[duplicated_data, cols])
    data <- data[ !duplicated_data, ]
  } 
  
  return(data)
}

flatten_long_to_wide = function(columns_to_flatten, longToWideFormula, longToWideIdKeyColumns, data, columns_agg_function = NULL){
  library(futile.logger)
  flog.info("Running function flatten_long_to_wide")
  #   if (!is.null(columns_agg_function)){
  #     df_flattened_so_far <- dcast(data,  longToWideFormula, value.var=columns_to_flatten[1], fun.aggregate= columns_agg_function[[1]] )  
  #   }      
  #   else{
  #     df_flattened_so_far <- dcast(data,  longToWideFormula, value.var=columns_to_flatten[1] )  
  #   }
  #   
  #   #assign correct rownames and remove row name column
  #   rownames(df_flattened_so_far) <- df_flattened_so_far$RPT  
  #   df_flattened_so_far <- subset(df_flattened_so_far, select=-c(RPT))
  #   print(paste("is agg function null", is.null(columns_agg_function)))
  #   
  #   
  #   if (length(columns_to_flatten) == 1){
  #     return (df_flattened_so_far)
  #   }
  
  df_flattened_so_far = NULL
  for(i in 1:length(columns_to_flatten)){
    c = columns_to_flatten[i]
    
    #cast long to wide, value Start Event Date period value in days
    if (!is.null(columns_agg_function)){
      df_temp <- dcast(data,  longToWideFormula, value.var=c, fun.aggregate=columns_agg_function[[i]] )  
    }      
    else{
      df_temp <- dcast(data,  longToWideFormula, value.var=c )  
    }  
    
    #ensure character columns are converted to factors
    cols_widened = colnames(df_temp)[ which(!colnames(df_temp) %in% longToWideIdKeyColumns)]
    df_temp[, cols_widened] <- lapply(df_temp[, cols_widened], function(x){     
      if (is.character(x)) return(as.factor(x))
      return(x)
    })
    
    #correct row names
    rownames(df_temp) <- df_temp$RPT  
    df_temp <- subset(df_temp, select=-c(RPT))
    
    #     ##Debug
    #     print("#Debug - str(df_flattened_so_far)")
    #     print(str(df_flattened_so_far))
    #     print("#Debug - str(df_temp)")
    #     print(str(df_temp))
    
    
    if (is.null(df_flattened_so_far)){
      df_flattened_so_far <- df_temp
    }else {
      #merge newly casted df and the previous df 
      df_flattened_so_far <- merge(df_flattened_so_far, df_temp, by=0, all.x=TRUE, suffixes=c(paste("_", previous_col, sep=""), paste("_", c, sep="" ) ))
      flog.trace(str(df_flattened_so_far,  list.len = 999))
      #assign correct rownames and remove row name column
      rownames(df_flattened_so_far) <- df_flattened_so_far$Row.names  
      df_flattened_so_far <- subset(df_flattened_so_far, select=-c(Row.names)) 
      
    }
    previous_col=c
    
  }
  
  return(df_flattened_so_far)
}


