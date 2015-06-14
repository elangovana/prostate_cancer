##########
library(futile.logger)

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

setup_outdir <- function(outdir){
  if (!file.exists(outdir)){  
    dir.create(file.path(".", outdir)) 
  }
  cur_time=format(Sys.time(), "%Y%b%d_%H%M%S")
  outdir = file.path(outdir, cur_time)
  dir.create(outdir, cur_time)  
  return(outdir)
}

## main
out_dir ="./outdat_trainmode"
out_dir <- setup_outdir(out_dir)

sink()
setup_log(out_dir)
flog.threshold(INFO)
set_options()

input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")


#rows_in_train = c(1100:1300,1401:1600)
#rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)
rows_in_train = c(1:200, 301:700, 801:1000)
rows_in_test = c(201:300,701:800,1301:1400)
#rows_in_test = c(1301:1400)
#rows_in_train = c(1:200)
rows_in_test = c(201:300)

####Download data#############

CoreTable_synapse_entity <- file.path(input_data_train_dir,"CoreTable_training.csv") 
CoreTable_training <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_train_dir,"LabValue_training.csv") 
LabValue_training <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT"))

LesionMeasure_synapse_entity <- file.path(input_data_train_dir, "LesionMeasure_training.csv")
LesionMeasure_training <- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".", as.is=c("RPT"))

MedHistory_synapse_entity <- file.path(input_data_train_dir,"MedHistory_training.csv")
MedHistory_training <- read.csv(MedHistory_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))

PriorMed_synapse_entity <- file.path(input_data_train_dir,"PriorMed_training.csv")
PriorMed_training <- read.csv(PriorMed_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))

VitalSign_synapse_entity <- file.path(input_data_train_dir,"VitalSign_training.csv")
VitalSign_training <- read.csv(VitalSign_synapse_entity, header=T, na.strings=c(".", ""), as.is=c("RPT"))

##

## Split training data into training and test
train_ct <- CoreTable_training[rows_in_train, ]
train_lv <- LabValue_training[LabValue_training$RPT %in% rownames(train_ct), ]
train_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(train_ct), ]
train_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(train_ct), ]
train_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(train_ct), ]
train_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(train_ct), ]

test_ct <- CoreTable_training[rows_in_test, !colnames(CoreTable_training) %in% c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC") ]
test_lv <- LabValue_training[LabValue_training$RPT %in%  rownames(test_ct), ]
test_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(test_ct), ]
test_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(test_ct), ]
test_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(test_ct), ]
test_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(test_ct), ]

run_round1_pipeline <- function(){  
  source("./ml_pipeline.R")
  result <- ml_pipeline(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                        test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted <- result$df_predicted
  
  source("./score.R")
  
  
  #RMSE test
  rmse_test = score_q1b(df_predicted$LKADT_P,CoreTable_training[rownames(df_predicted), c("LKADT_P")], CoreTable_training[rownames(df_predicted), c("DEATH")])
  #RMSE Train
  train_predictions_ttl = result$model_ttl$fit$predicted  
  rmse_train = score_q1b(train_predictions_ttl,CoreTable_training[names(train_predictions_ttl), c("LKADT_P")], CoreTable_training[names(train_predictions_ttl), c("DEATH")])
  write.csv(data.frame(names=names(train_predictions_ttl), actual=CoreTable_training[names(train_predictions_ttl), c("LKADT_P")], prdicted=train_predictions_ttl), file=file.path(out_dir, "predicted_vs_actual_ttl.csv"))
  source("./translate_data.R")
  cleaned_train = clean_labels(CoreTable_training[rownames(df_predicted), ])
  percentage_correct_death = (100 * length(which(as.character(df_predicted$DEATH)== as.character(cleaned_train[rownames(df_predicted), c("DEATH")])))) / length(df_predicted$DEATH)
  
   
  print(paste("RMSE on test: ", rmse_test, "RMSE on train:",  rmse_train, "% predicted corect death ", percentage_correct_death))
  
  #risk train
  risk_score_global <- result$risk_score_global$train
  risk_score_12 <- result$risk_score_12$train
  risk_score_18 <- result$risk_score_18$train
  risk_score_24 <- result$risk_score_24$train
  risk_score_train <- score_q1a(CoreTable_training[names(risk_score_global), c("LKADT_P")],CoreTable_training[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])
  
  
  
  risk_score_global <- result$risk_score_global$test
  risk_score_12 <- result$risk_score_12$test
  risk_score_18 <- result$risk_score_18$test
  risk_score_24 <- result$risk_score_24$test
 
  risk_score_test <- score_q1a(CoreTable_training[names(risk_score_global), c("LKADT_P")],CoreTable_training[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])
  
  print("-----OUTPUT---")
  #RMSE Comparitive output
  print(paste("RMSE on test: ", rmse_test, "RMSE on train:",  rmse_train, "% predicted corect death ", percentage_correct_death))
  print("Global risk score on train: ")
  print(risk_score_train)
  print("Global risk score on test: ")
  print(risk_score_test)
}

run_round2_pipeline<- function(){  
  source("./ml_pipeline.R")
  result <- ml_pipeline_part2(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted <- result$df_predicted
  percentage_correct = (100 * length(which(df_predicted$DISCONT == as.factor(CoreTable_training[rownames(df_predicted), c("DISCONT")])))) / length(df_predicted$DISCONT)
  print("Output")
  print(paste("Percentage correct prediction for discontinued flag", percentage_correct, sep=" "))
}


run_round3_pipeline <- function(){
  source("./ml_pipeline.R")
  result <- ml_pipeline_part3(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
                              test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)
  df_predicted <- result$df_predicted
  percentage_correct = (100 * length(which(df_predicted$ENDTRS_C == as.factor(CoreTable_training[rownames(df_predicted), c("ENDTRS_C")])))) / length(df_predicted$ENDTRS_C)
  print("Output")
  print(paste("Percentage correct prediction for discontinued reason", percentage_correct, sep=" "))
}

run_round1_pipeline()
#run_round2_pipeline()
#run_round3_pipeline()
warnings()
