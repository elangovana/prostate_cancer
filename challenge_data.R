source("./generic_s3_methods.R")

challenge_data <- function(input_dir, file_suffix, ct_rows_to_extract=NULL){
  
  CoreTable_synapse_entity <- file.path(input_dir, paste("CoreTable",  file_suffix, sep="")) 
  CoreTable <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))
  
  LabValue_synapse_entity <- file.path(input_dir, paste("LabValue",  file_suffix, sep="") )
  LabValue <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT"))
  
  LesionMeasure_synapse_entity <- file.path(input_dir, paste("LesionMeasure",  file_suffix, sep=""))
  LesionMeasure <- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".", as.is=c("RPT"))
  
  MedHistory_synapse_entity <- file.path(input_dir,paste("MedHistory",  file_suffix, sep=""))
  MedHistory <- read.csv(MedHistory_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))
  
  PriorMed_synapse_entity <- file.path(input_dir, paste("PriorMed",  file_suffix, sep=""))
  PriorMed <- read.csv(PriorMed_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))
  
  VitalSign_synapse_entity <- file.path(input_dir, paste("VitalSign",  file_suffix, sep=""))
  VitalSign <- read.csv(VitalSign_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))
  
  #if only specific rows have to extracted
  if ( !is.null(ct_rows_to_extract)){
    CoreTable <- CoreTable[ct_rows_to_extract, ]
    LabValue <- LabValue[LabValue$RPT %in% rownames(CoreTable), ]
    LesionMeasure <- LesionMeasure[LesionMeasure$RPT %in% rownames(CoreTable), ]
    MedHistory <- MedHistory[MedHistory$RPT %in% rownames(CoreTable), ]
    PriorMed <- PriorMed[PriorMed$RPT %in% rownames(CoreTable), ]
    VitalSign <- VitalSign[VitalSign$RPT %in% rownames(CoreTable), ]    
  }
  
  #separate out y cols
  ycolnames <- c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC")
  
  out <- list(ct = CoreTable[, !colnames(CoreTable) %in%  ycolnames], 
              lv = LabValue,
              lm = LesionMeasure,
              mh = MedHistory, 
              pm = PriorMed,
              vs = VitalSign,              
              ycols = if(sum(!is.na(CoreTable[, ycolnames]))!=0){ CoreTable[, ycolnames]} else{ NULL },
              merged_data = NULL)
  
  class(out) <- "challenge_data"  
  invisible(out)
}

print.challenge_data <- function(object){
  print("-- core table --")
  print(str(object$ct))
  print("-- lab value --")
  print(str(object$lv))
  
  print("--y cols--")
  print(str(object$ycols))
}

summary.challenge_data <- function(object){
  print("-- core table dim --")
  print(dim(object$ct))
  print("-- lab value dim--")
  print(dim(object$lv))
  print("-- lesion measure dim--")
  print(dim(object$lm))
  print("-- medical history dim--")
  print(dim(object$mh))
  print("-- vital signs dim--")
  print(dim(object$vs))
  print("-- prior med dim--")
  print(dim(object$pm))
  print("-- y cols dim--")
  print(dim(object$ycols))
}

#general data cleanup, nothing fancy here :-!
cleanup.challenge_data <- function(object){
  library(futile.logger)
  source("./translate_data.R")
  
  flog.info("cleaning data")  
  flog.info("No of  records before clean up %s",nrow(object$ct) )
  
  object$ct <- clean_ct_data (object$ct)
  object$lv <- clean_labvalue_data (object$lv)
  object$lm <- clean_lesionmeasure_data(object$lm)
  object$mh <- clean_medical_history(object$mh)
  object$vs <- clean_vital_signs(object$vs)
  object$pm <- clean_prior_medicals(object$pm)
  
  if (!is.null(object$ycols)){
    object$ycols <- clean_labels(object$ycols)
  }
  
  flog.info("No of  records after clean up %s",nrow(object$ct) )  
  return(object)
}

rbind.challenge_data  <- function(object, cdata2){
  object$ct <- rbind(object$ct, cdata2$ct)
  object$lv <- rbind(object$lv, cdata2$lv)
  object$lm <- rbind(object$lm, cdata2$lm)
  object$vs <- rbind(object$vs, cdata2$vs)
  object$pm <- rbind(object$pm, cdata2$pm)
  object$mh <- rbind(object$mh, cdata2$mh)
  
  object$merged_data <- NULL
  object$ycols <- rbind(object$ycols, cdata2$ycols)
    
  return(object)
}

#merges multiple challenge data sets into one
merge.challenge_data <- function(object){
  
  df_subset_merged <- merge(object$ct, object$lv, by=0, all.x=TRUE, suffixes= c(".ct", ".lv" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))    
  
  #merge Lesion measure
  df_subset_merged <- merge(df_subset_merged, object$lm, by=0, all.x=TRUE, suffixes= c(".ctlv", ".lm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #   #merge medical history
  df_subset_merged <- merge(df_subset_merged, object$mh, by=0, all.x=TRUE, suffixes= c(".ctlvlm", ".mh" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  #   
  #   #merge vital signs
  df_subset_merged <- merge(df_subset_merged, object$vs, by=0, all.x=TRUE, suffixes= c(".ctlvlmmh", ".vs" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  #   
  #   #merge prior medications
  df_subset_merged <- merge(df_subset_merged, object$pm, by=0, all.x=TRUE, suffixes= c(".ctlvlmmhpm", ".pm" ))
  rownames(df_subset_merged) <- df_subset_merged$Row.names
  df_subset_merged <- subset(df_subset_merged, select=-c(Row.names))
  
  object$merged_data <- df_subset_merged
  return(object)
  
}

