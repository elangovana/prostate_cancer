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
clean_labels <- function (train_ct){
  #Death
  train_ct$DEATH <- as.factor(train_ct$DEATH)
  levels(train_ct$DEATH) <- c("YES", "CENSORED")
  train_ct$DEATH[is.na( train_ct$DEATH)] <- "CENSORED"   
  
  
  return(train_ct)
}

remove_duplicated_record <- function(data, cols){
  #remove duplicated lab results
    duplicated_data <- duplicated(data[,  cols])
    duplicate_count <-length (duplicated_data[duplicated_data == TRUE])
    
    if ( duplicate_count > 0){
      warning(paste("Duplicates found:", duplicate_count,  "These will be removed!!", sep = " " ))
      print("Duplicate lab result record keys :")
      print(data[duplicated_data, cols])
      data <- data[ !duplicated_data, ]
    } 
    
    return(data)
}

flatten_long_to_wide = function(columns_to_flatten, longToWideFormula, longToWideIdKeyColumns, data, columns_agg_function = NULL){
  
  df_flattened_so_far <- dcast(data,  longToWideFormula, value.var=columns_to_flatten[1] )
  #assign correct rownames and remove row name column
  rownames(df_flattened_so_far) <- df_flattened_so_far$RPT  
  df_flattened_so_far <- subset(df_flattened_so_far, select=-c(RPT))
  
  for(i in 2:length(columns_to_flatten)){
    c = columns_to_flatten[i]
    #cast long to wide, value Start Event Date period value in days
    if (!is.null(columns_agg_function)){
      df_temp <- dcast(data,  longToWideFormula, value.var=columns_agg_function[i] )  
    }      
    else{
      df_temp <- dcast(data,  longToWideFormula )  
    }
      
    
    #ensure character columns are converted to factors
    cols_widened = !colnames(df_temp) %in% longToWideIdKeyColumns
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
    
    #merge newly casted df and the previous df 
    df_flattened_so_far <- merge(df_flattened_so_far, df_temp, by=0, all.x=TRUE, suffixes=c("", paste("_", c, sep="" ) ))
    
    #assign correct rownames and remove row name column
    rownames(df_flattened_so_far) <- df_flattened_so_far$Row.names  
    df_flattened_so_far <- subset(df_flattened_so_far, select=-c(Row.names))   
  }
  return(df_flattened_so_far)
}

clean_ct_data <- function(train_ct){
  
  print(head(rownames(train_ct)))
  #format age group
  train_ct$AGEGRP <- as.numeric(train_ct$AGEGRP)
  
  #age group 2
  
  #RACE_C
  train_ct$RACE_C <- as.factor(train_ct$RACE_C)
  train_ct$RACE_C[train_ct$RACE_C == 'Missing'] <- NA
  levels(train_ct$RACE_C) <- c("Asian", "Black", "White", "Hispanic", "Other", "Missing")
  
  #RACE_C
  train_ct$REGION_C <- as.factor(train_ct$REGION_C)
  train_ct$REGION_C[train_ct$REGION_C == 'Missing'] <- NA
  levels(train_ct$REGION_C) <- c("ASIA/PACIFIC", " WESTERN EUROPE", "EASTERN EUROPE", "SOUTH AMERICA", "NORTH AMERICA", "OTHER", "MISSING")
  
  
  #NON_TARGET
  levels(train_ct$NON_TARGET) <- c("YES", "NO")
  train_ct$NON_TARGET[is.na( train_ct$NON_TARGET)] <- "NO"
  
  #Target Column
  levels(train_ct$TARGET) <- c("YES", "NO")
  train_ct$TARGET[is.na( train_ct$TARGET)] <- "NO"   
  
  #BONE Column
  levels(train_ct$BONE) <- c("YES", "NO")
  train_ct$BONE[is.na( train_ct$BONE)] <- "NO"
  
  #RECTAL
  levels(train_ct$RECTAL) <- c("YES", "NO")
  train_ct$RECTAL[is.na( train_ct$RECTAL)] <- "NO"
  
  #    LYMPH_NODES
  levels(train_ct$LYMPH_NODES) <- c("YES", "NO")
  train_ct$LYMPH_NODES[is.na( train_ct$LYMPH_NODES)] <- "NO"
  
  #    KIDNEYS
  levels(train_ct$KIDNEYS) <- c("YES", "NO")
  train_ct$KIDNEYS[is.na( train_ct$KIDNEYS)] <- "NO"
  
  #    LUNGS
  levels(train_ct$LUNGS) <- c("YES", "NO")
  train_ct$LUNGS[is.na( train_ct$LUNGS)] <- "NO"
  
  #    LIVER
  levels(train_ct$LIVER) <- c("YES", "NO")
  train_ct$LIVER[is.na( train_ct$LIVER)] <- "NO"
  
  #    PLEURA
  levels(train_ct$PLEURA) <- c("YES", "NO")
  train_ct$PLEURA[is.na( train_ct$PLEURA)] <- "NO"
  
  #    OTHER
  levels(train_ct$OTHER) <- c("YES", "NO")
  train_ct$OTHER[is.na( train_ct$OTHER)] <- "NO"
  
  #    PROSTATE
  levels(train_ct$PROSTATE) <- c("YES", "NO")
  train_ct$PROSTATE[is.na( train_ct$PROSTATE)] <- "NO"
  
  #    ADRENAL
  levels(train_ct$ADRENAL) <- c("YES", "NO")
  train_ct$ADRENAL[is.na( train_ct$ADRENAL)] <- "NO"
  
  #    BLADDER
  levels(train_ct$BLADDER) <- c("YES", "NO")
  train_ct$BLADDER[is.na( train_ct$BLADDER)] <- "NO"
  
  #    PERITONEUM
  levels(train_ct$PERITONEUM) <- c("YES", "NO")
  train_ct$PERITONEUM[is.na( train_ct$PERITONEUM)] <- "NO"
  
  #    COLON
  train_ct$COLON <- as.factor(train_ct$COLON)
  levels(train_ct$COLON) <- c("YES", "NO")
  train_ct$COLON[is.na( train_ct$COLON)] <- "NO"
  
  #    HEAD_AND_NECK
  train_ct$HEAD_AND_NECK <- as.factor(train_ct$HEAD_AND_NECK)
  levels(train_ct$HEAD_AND_NECK) <- c("YES", "NO")
  train_ct$HEAD_AND_NECK[is.na( train_ct$HEAD_AND_NECK)] <- "NO"
  
  
  #    SOFT_TISSUE
  train_ct$SOFT_TISSUE <- as.factor(train_ct$SOFT_TISSUE)
  levels(train_ct$SOFT_TISSUE) <- c("YES", "NO")
  train_ct$SOFT_TISSUE[is.na( train_ct$SOFT_TISSUE)] <- "NO"
  
  #    STOMACH
  levels(train_ct$STOMACH) <- c("YES", "NO")
  train_ct$STOMACH[is.na( train_ct$STOMACH)] <- "NO"
  
  #    PANCREAS
  train_ct$PANCREAS  <- as.factor(train_ct$PANCREAS)
  levels(train_ct$PANCREAS) <- c("YES", "NO")
  train_ct$PANCREAS[is.na( train_ct$PANCREAS)] <- "NO"
  
  
  #    THYROID
  train_ct$THYROID  <- as.factor(train_ct$THYROID)
  levels(train_ct$THYROID) <- c("YES", "NO")
  train_ct$THYROID[is.na( train_ct$THYROID)] <- "NO"
  
  
  #    ABDOMINAL
  train_ct$ABDOMINAL  <- as.factor(train_ct$ABDOMINAL)
  levels(train_ct$ABDOMINAL) <- c("YES", "NO")
  train_ct$ABDOMINAL[is.na( train_ct$ABDOMINAL)] <- "NO"
  
  #    ORCHIDECTOMY
  levels(train_ct$ORCHIDECTOMY) <- c("YES", "NO")
  train_ct$ORCHIDECTOMY[is.na( train_ct$ORCHIDECTOMY)] <- "NO"
  
  #    PROSTATECTOMY
  levels(train_ct$PROSTATECTOMY) <- c("YES", "NO")
  train_ct$PROSTATECTOMY[is.na( train_ct$PROSTATECTOMY)] <- "NO"
  
  #    TURP
  levels(train_ct$TURP) <- c("YES", "NO")
  train_ct$TURP[is.na( train_ct$TURP)] <- "NO"
  
  #    LYMPHADENECTOMY
  levels(train_ct$LYMPHADENECTOMY) <- c("YES", "NO")
  train_ct$LYMPHADENECTOMY[is.na( train_ct$LYMPHADENECTOMY)] <- "NO"
  
  #    SPINAL_CORD_SURGERY
  levels(train_ct$SPINAL_CORD_SURGERY) <- c("YES", "NO")
  train_ct$SPINAL_CORD_SURGERY[is.na( train_ct$SPINAL_CORD_SURGERY)] <- "NO"
  
  #    BILATERAL_ORCHIDECTOMY
  levels(train_ct$BILATERAL_ORCHIDECTOMY) <- c("YES", "NO")
  train_ct$BILATERAL_ORCHIDECTOMY[is.na( train_ct$BILATERAL_ORCHIDECTOMY)] <- "NO"
  
  
  #    PRIOR_RADIOTHERAPY
  levels(train_ct$PRIOR_RADIOTHERAPY) <- c("YES", "NO")
  train_ct$PRIOR_RADIOTHERAPY[is.na( train_ct$PRIOR_RADIOTHERAPY)] <- "NO"
  
  #   ANALGESICS
  levels(train_ct$ANALGESICS) <- c("YES", "NO")
  train_ct$ANALGESICS[is.na( train_ct$ANALGESICS)] <- "NO"
  
  #   ANTI_ANDROGENS
  levels(train_ct$ANTI_ANDROGENS) <- c("YES", "NO")
  train_ct$ANTI_ANDROGENS[is.na( train_ct$ANTI_ANDROGENS)] <- "NO"
  
  #   GLUCOCORTICOID
  levels(train_ct$GLUCOCORTICOID) <- c("YES", "NO")
  train_ct$GLUCOCORTICOID[is.na( train_ct$GLUCOCORTICOID)] <- "NO"
  
  #   GONADOTROPIN
  levels(train_ct$GONADOTROPIN) <- c("YES", "NO")
  train_ct$GONADOTROPIN[is.na( train_ct$GONADOTROPIN)] <- "NO"
  
  
  #   BISPHOSPHONATE
  levels(train_ct$BISPHOSPHONATE) <- c("YES", "NO")
  train_ct$BISPHOSPHONATE[is.na( train_ct$BISPHOSPHONATE)] <- "NO"
  
  #   CORTICOSTEROID
  levels(train_ct$CORTICOSTEROID) <- c("YES", "NO")
  train_ct$CORTICOSTEROID[is.na( train_ct$CORTICOSTEROID)] <- "NO"
  
  #   IMIDAZOLE
  levels(train_ct$IMIDAZOLE) <- c("YES", "NO")
  train_ct$IMIDAZOLE[is.na( train_ct$IMIDAZOLE)] <- "NO"
  
  #   ACE_INHIBITORS
  levels(train_ct$ACE_INHIBITORS) <- c("YES", "NO")
  train_ct$ACE_INHIBITORS[is.na( train_ct$ACE_INHIBITORS)] <- "NO"
  
  #   BETA_BLOCKING
  levels(train_ct$BETA_BLOCKING) <- c("YES", "NO")
  train_ct$BETA_BLOCKING[is.na( train_ct$BETA_BLOCKING)] <- "NO"
  
  #   HMG_COA_REDUCT
  levels(train_ct$HMG_COA_REDUCT) <- c("YES", "NO")
  train_ct$HMG_COA_REDUCT[is.na( train_ct$HMG_COA_REDUCT)] <- "NO"
  
  #   ESTROGENS
  levels(train_ct$ESTROGENS) <- c("YES", "NO")
  train_ct$ESTROGENS[is.na( train_ct$ESTROGENS)] <- "NO"
  
  #   ANTI_ESTROGENS
  train_ct$ANTI_ESTROGENS <- as.factor(train_ct$ANTI_ESTROGENS)
  levels(train_ct$ANTI_ESTROGENS) <- c("YES", "NO")
  train_ct$ANTI_ESTROGENS[is.na( train_ct$ANTI_ESTROGENS)] <- "NO"
  
  #   ARTTHROM
  train_ct$ARTTHROM <- as.factor(train_ct$ARTTHROM)
  levels(train_ct$ARTTHROM) <- c("YES", "NO")
  train_ct$ARTTHROM[is.na( train_ct$ARTTHROM)] <- "NO"
  
  #   CEREBACC
  levels(train_ct$CEREBACC) <- c("YES", "NO")
  train_ct$CEREBACC[is.na( train_ct$CEREBACC)] <- "NO"
  
  #   CHF
  levels(train_ct$CHF) <- c("YES", "NO")
  train_ct$CHF[is.na( train_ct$CHF)] <- "NO"
  
  #   DVT
  levels(train_ct$DVT) <- c("YES", "NO")
  train_ct$DVT[is.na( train_ct$DVT)] <- "NO"
  
  #   DIAB
  levels(train_ct$DIAB) <- c("YES", "NO")
  train_ct$DIAB[is.na( train_ct$DIAB)] <- "NO"
  
  #   GASTREFL
  levels(train_ct$GASTREFL) <- c("YES", "NO")
  train_ct$GASTREFL[is.na( train_ct$GASTREFL)] <- "NO"
  
  #   GIBLEED
  train_ct$GIBLEED <- as.factor(train_ct$GIBLEED)
  levels(train_ct$GIBLEED) <- c("YES", "NO")
  train_ct$GIBLEED[is.na( train_ct$GIBLEED)] <- "NO"
  
  #   MI
  levels(train_ct$MI) <- c("YES", "NO")
  train_ct$MI[is.na( train_ct$MI)] <- "NO"
  
  #   PUD
  levels(train_ct$PUD) <- c("YES", "NO")
  train_ct$PUD[is.na( train_ct$PUD)] <- "NO"
  
  #   PULMEMB
  levels(train_ct$PULMEMB) <- c("YES", "NO")
  train_ct$PULMEMB[is.na( train_ct$PULMEMB)] <- "NO"
  
  #   PATHFRAC
  levels(train_ct$PATHFRAC) <- c("YES", "NO")
  train_ct$PATHFRAC[is.na( train_ct$PATHFRAC)] <- "NO"
  
  #   SPINCOMP
  levels(train_ct$SPINCOMP) <- c("YES", "NO")
  train_ct$SPINCOMP[is.na( train_ct$SPINCOMP)] <- "NO"
  
  #   COPD
  levels(train_ct$COPD) <- c("YES", "NO")
  train_ct$COPD[is.na( train_ct$COPD)] <- "NO"
  
  #   MHBLOOD
  levels(train_ct$MHBLOOD) <- c("YES", "NO")
  train_ct$MHBLOOD[is.na( train_ct$MHBLOOD)] <- "NO"
  
  #   MHCARD
  levels(train_ct$MHCARD) <- c("YES", "NO")
  train_ct$MHCARD[is.na( train_ct$MHCARD)] <- "NO"
  
  #   MHCONGEN
  levels(train_ct$MHCONGEN) <- c("YES", "NO")
  train_ct$MHCONGEN[is.na( train_ct$MHCONGEN)] <- "NO"
  
  #   MHEAR
  levels(train_ct$MHEAR) <- c("YES", "NO")
  train_ct$MHEAR[is.na( train_ct$MHEAR)] <- "NO"
  
  #   MHENDO
  levels(train_ct$MHENDO) <- c("YES", "NO")
  train_ct$MHENDO[is.na( train_ct$MHENDO)] <- "NO"
  
  #   MHEYE
  levels(train_ct$MHEYE) <- c("YES", "NO")
  train_ct$MHEYE[is.na( train_ct$MHEYE)] <- "NO"
  
  #   MHGASTRO
  levels(train_ct$MHGASTRO) <- c("YES", "NO")
  train_ct$MHGASTRO[is.na( train_ct$MHGASTRO)] <- "NO"
  
  
  
  #   MHGEN
  levels(train_ct$MHGEN) <- c("YES", "NO")
  train_ct$MHGEN[is.na( train_ct$MHGEN)] <- "NO"
  
  #   MHHEPATO
  levels(train_ct$MHHEPATO) <- c("YES", "NO")
  train_ct$MHHEPATO[is.na( train_ct$MHHEPATO)] <- "NO"
  
  #   MHIMMUNE
  levels(train_ct$MHIMMUNE) <- c("YES", "NO")
  train_ct$MHIMMUNE[is.na( train_ct$MHIMMUNE)] <- "NO"
  
  #   MHINFECT
  levels(train_ct$MHINFECT) <- c("YES", "NO")
  train_ct$MHINFECT[is.na( train_ct$MHINFECT)] <- "NO"
  
  #   MHINJURY
  levels(train_ct$MHINJURY) <- c("YES", "NO")
  train_ct$MHINJURY[is.na( train_ct$MHINJURY)] <- "NO"
  
  #   MHINVEST
  levels(train_ct$MHINVEST) <- c("YES", "NO")
  train_ct$MHINVEST[is.na( train_ct$MHINVEST)] <- "NO"
  
  #   MHMETAB
  levels(train_ct$MHMETAB) <- c("YES", "NO")
  train_ct$MHMETAB[is.na( train_ct$MHMETAB)] <- "NO"
  
  #   MHMUSCLE
  levels(train_ct$MHMUSCLE) <- c("YES", "NO")
  train_ct$MHMUSCLE[is.na( train_ct$MHMUSCLE)] <- "NO"
  
  #   MHNEOPLA
  levels(train_ct$MHNEOPLA) <- c("YES", "NO")
  train_ct$MHNEOPLA[is.na( train_ct$MHNEOPLA)] <- "NO"
  
  #   MHNERV
  levels(train_ct$MHNERV) <- c("YES", "NO")
  train_ct$MHNERV[is.na( train_ct$MHNERV)] <- "NO"
  
  #   MHPSYCH
  levels(train_ct$MHPSYCH) <- c("YES", "NO")
  train_ct$MHPSYCH[is.na( train_ct$MHPSYCH)] <- "NO"
  
  #   MHRENAL
  levels(train_ct$MHRENAL) <- c("YES", "NO")
  train_ct$MHRENAL[is.na( train_ct$MHRENAL)] <- "NO"
  
  #   MHRESP
  levels(train_ct$MHRESP) <- c("YES", "NO")
  train_ct$MHRESP[is.na( train_ct$MHRESP)] <- "NO"
  
  #   MHSKIN
  levels(train_ct$MHSKIN) <- c("YES", "NO")
  train_ct$MHSKIN[is.na( train_ct$MHSKIN)] <- "NO"
  
  #   MHSOCIAL
  levels(train_ct$MHSOCIAL) <- c("YES", "NO")
  train_ct$MHSOCIAL[is.na( train_ct$MHSOCIAL)] <- "NO"
  
  #   MHSURG
  levels(train_ct$MHSURG) <- c("YES", "NO")
  train_ct$MHSURG[is.na( train_ct$MHSURG)] <- "NO"
  
  
  
  #   MHVASC
  levels(train_ct$MHVASC) <- c("YES", "NO")
  train_ct$MHVASC[is.na( train_ct$MHVASC)] <- "NO"
  
  return(train_ct)
  
}


clean_labvalue_data <- function(labvalue_data){
  library(reshape2)
  print("--- begin function clean_labvalue_data ----")
  
  #clean up data
  #remove rows with NA labresult
  labvalue_result <- labvalue_data[ !is.na(labvalue_data$LBSTRESN), ]
  
  #If lab code is NA, replace with lab test description  
  levels(labvalue_result$LBTESTCD) <- c(levels(labvalue_result$LBTESTCD), as.character(unique(labvalue_result$LBTEST[is.na(labvalue_result$LBTESTCD)])))
  labvalue_result$LBTESTCD[is.na(labvalue_result$LBTESTCD)] <- as.character(labvalue_result$LBTEST[is.na(labvalue_result$LBTESTCD)])
  
  
  #remove duplicated lab results
  #   duplicate_lab_results <- duplicated(labvalue_result[,  c("RPT", "LBTESTCD", "VISIT")])
  #   duplicate_count <-length (duplicate_lab_results[duplicate_lab_results == TRUE])
  #   if ( duplicate_count > 0){
  #     warning(paste("Duplicate lab results found:", duplicate_count,  "These will be removed!!", sep = " " ))
  #     print("Duplicate lab result record keys :")
  #     print(labvalue_result[duplicate_lab_results, c("RPT", "LBTESTCD", "VISIT")])
  #     labvalue_result <- labvalue_result[ !duplicate_lab_results, ]
  #   }
  
  #cast long to wide
  labvalue_result <- dcast(labvalue_result,  DOMAIN + STUDYID + RPT ~ LBTESTCD, value.var="LBSTRESN" , fun.aggregate = mean)
  
  #assign correct rownames and remove row name column
  rownames(labvalue_result) <- labvalue_result$RPT  
  labvalue_result <- subset(labvalue_result, select=-c(RPT))
  
  #clean up column names to remove -, # white space, replaced with _  
  colnames(labvalue_result) <-clean_names(colnames(labvalue_result))  
  
  print("Tranformed lab value structure")
  print(str(labvalue_result))
  
  print("--- end clean_labvalue_data ----")
  return(labvalue_result)
}


clean_lesionmeasure_data <- function(labmeasure_data){
  library(reshape2)
  print("--- begin function clean_labmeasure_data ----")
  print(str(labmeasure_data))
  
  if (nrow(labmeasure_data) == 0){
    warning("Lab measure data frame empty. Hence lab measure values will not be used")
    return (labmeasure_data)
  }
  
  #cast long to wide
  #DOMAIN + STUDYID + 
  labvalue_result <- dcast(labmeasure_data,  RPT ~ LSTEST + LSLOC, value.var="LSSTRESN" , fun.aggregate = mean)
  
  
  #assign correct rownames and remove row name column
  rownames(labvalue_result) <- labvalue_result$RPT  
  labvalue_result <- subset(labvalue_result, select=-c(RPT))
  
  
  
  
  #clean up column names to remove -, # white space, replaced with _
  colnames(labvalue_result) <-clean_names( colnames(labvalue_result))
  
  
  print(str(labvalue_result))
  print("--- end clean_labmeasure_data ----")
  return(labvalue_result)
}

clean_medical_history<- function(data){
  print("--- begin function clean_medical_history ----")
  
  library(reshape2)
  print("Input medical history data structure")
  print(str(data))
  
  if (nrow(data) == 0){
    warning("Medical history data frame empty. Hence medical history values will not be used")
    return (data)
  }
  
  #If mhde code is NA, replace with description  
  levels(data$MHDECOD) <- c(levels(data$MHDECOD), as.character(unique(data$MHTERM[is.na(data$MHDECOD)])))
  data$MHDECOD[is.na(data$MHDECOD)] <- as.character(data$MHTERM[is.na(data$MHDECOD)])
  
  #"DOMAIN" , "STUDYID",
  longToWideFormula <- as.formula("RPT ~ MHDECOD + MHBODSYS" )
  longToWideIdKeyColumns = c(  "RPT", "MHDECOD" ,"MHBODSYS" )
  
 
  data <- remove_duplicated_record(data, longToWideIdKeyColumns)
  
  #all value to cast to wide
  columns_to_flatten <- c ("MHSTDT_P"                          
                           #,"MHDICTV"
                           ,"MHHLGT"
                           ,"MHLLT" 
                           ,"MHHLT"                          
                           , "MHBODSYS"
                           ,"MHOCCUR"
                           ,"MHPRESP"
                           ,"MHENRF"
                           ,"MHENRFO"
#                           ,"VISIT"
#                           ,"VISITNUM"
                           #,"MHCAT"
                           #,"MHCONTR"
                           #,"MHPTCD"
                           #,"MHSOC1FL"
#                           ,"MHSOCCD"
#                            ,"MHHLGTCD"
#                            ,"MHHLTCD"
#                            ,"MHLLTCD"
  )

  
  data <- flatten_long_to_wide(columns_to_flatten, longToWideFormula, longToWideIdKeyColumns, data)
  
  
  
  #clean up column names to remove -, # white space, replaced with _
  colnames(data) <-clean_names( colnames(data))
  
  print("Reshaped medical history")
  print(str(data, list.len = 999))
  
  print("--- end clean_medical_history ----")
  return(data)
}

clean_vital_signs<- function(data){
  print("--- Begin function clean_vital_signs ----")
  
  library(reshape2)
  print("Input vital signs data structure")
  print(str(data))
  
  if (nrow(data) == 0){
    warning("Vital signs data frame empty. Hence vital signs values will not be used")
    return (data)
  }
  
  #If mhde code is NA, replace with description  
  #levels(data$MHDECOD) <- c(levels(data$MHDECOD), as.character(unique(data$MHTERM[is.na(data$MHDECOD)])))
  #data$MHDECOD[is.na(data$MHDECOD)] <- as.character(data$MHTERM[is.na(data$MHDECOD)])
  
  #"DOMAIN" , "STUDYID",
  longToWideFormula <- as.formula("RPT ~ VSTESTCD " )
  longToWideIdKeyColumns = c(  "RPT", "VSTESTCD"  )
  
  
  #data <- remove_duplicated_record(data, longToWideIdKeyColumns)
  
  #all value to cast to wide
  columns_to_flatten <- c ("VSSTRESN"  )
  
  columns_agg_function <- c(mean )
  data <- flatten_long_to_wide(columns_to_flatten, longToWideFormula, longToWideIdKeyColumns, data)
  
  
  
  #clean up column names to remove -, # white space, replaced with _
  colnames(data) <-clean_names( colnames(data))
  
  print("Reshaped vital signs")
  print(str(data, list.len = 999))
  
  print("--- end clean_vital_signs ----")
  return(data)
}