library("futile.logger")
source("./generic_s3_methods.R")
source("./score.R")

plot_kaplan_meier_survival <- function(time, events, plotfile){
  library(survival)  
  kaplan_meier <- survfit(Surv(time, events) ~ 1)
  print(summary(kaplan_meier))
  pdf(plotfile)
  plot(kaplan_meier)
  dev.off()
  
}

most_frequent_factor <- function(x){
  if (length(x) == 0) return(("-"))
  result <- names(which.max(table(x)))
  return( result)
}

calc_dummy_score <- function(row, days, lowerdays=0){ 
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

save_rng <- function(savefile=tempfile()) {
  if (exists(".Random.seed"))  {
    oldseed <- get(".Random.seed", .GlobalEnv)
  } else stop("don't know how to save before set.seed() or r*** call")
  oldRNGkind <- RNGkind()
  save("oldseed","oldRNGkind",file=savefile)
  invisible(savefile)
}

restore_rng <- function(savefile) {
  load(savefile)
  do.call("RNGkind",as.list(oldRNGkind))  ## must be first!
  assign(".Random.seed", oldseed, .GlobalEnv)
}


get_seed <- function(index){
  if (!is.null(object$seed_files) ){
    restore_rng(object$seed_file[index])
  }else{
    set.seed(NULL)
    save_rng(file.path(object$out_dir, paste("cleanup.discontinued_classifier",index,".seed", sep="")))
    save_rng(file.path(object$rseeds_out_dir, paste("cleanup.discontinued_classifier",index,".seed", sep="")))
    
  }
}

set_options <- function(){
  options(warning.length = 5000)
  options(warn =1)
}

#set up logging
setup_log <- function(outdir){
  con <- file(file.path(outdir,"runall.log"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  
  appender.file(con)
  #layout <- layout.format('[~l] [~t] [~n.~f] ~m')
  #flog.layout(layout)
  
  return(con)
}

setup_outdir <- function(base_dir, sub_directory_prefix=""){
  if (!file.exists(base_dir)){  
    dir.create(file.path(".", base_dir)) 
  }
  cur_time=format(Sys.time(), "%Y%m%d_%H%M%S")
  outdir = file.path(base_dir, paste(sub_directory_prefix, cur_time, sep=""))
  dir.create(outdir, cur_time)  
  return(outdir)
}

merge_data_frame_by_rows <- function(df1, df2,  all = FALSE, all.x = all, all.y = all, suffixes=c(".x",".y")){
  m_df = merge(df1, df2, by=0, all = all, all.x = all.x, all.y = all.y, suffixes = suffixes)
  rownames(m_df) <- m_df$Row.names
  m_df <- subset.data.frame(m_df, select = -c(Row.names) )
  return(m_df)
}

merge_data_frame_with_named_vector <- function(df, named_vector, suffixes=c(".frame", ".vector")){
  df_named_vector <- data.frame(named_vector)
  rownames(df_named_vector) <- names(named_vector);
  return(merge_data_frame_by_rows(df, df_named_vector, suffixes=suffixes))
}

init_named_vector_from_data_frame <- function(df, init_value =0){
  nv <- rep(init_value, nrow(df))
  names(nv) <- rownames(df)
  return(nv)
}