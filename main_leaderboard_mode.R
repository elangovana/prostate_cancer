
#set up logging
setup_log <- function(outdir){
  con <- file(file.path(outdir,"runall.log"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  library(futile.logger)
  appender.file(con)
  #layout <- layout.format('[~l] [~t] [~n.~f] ~m')
  #flog.layout(layout)
  
  return(con)
}

setup_outdir <- function(outdir){
  if (!file.exists(outdir)){  
    dir.create(file.path(".", outdir)) 
  }
  cur_time=format(Sys.time(), "%Y%m%d_%H%M%S")
  outdir = file.path(outdir, cur_time)
  dir.create(outdir, cur_time)  
  return(outdir)
}

########## M A I N ####
out_dir = "./out_dat"
sink()
out_dir <- setup_outdir(out_dir)
setup_log(out_dir)
flog.threshold(INFO)

input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
input_data_leaderboard_dir = file.path(input_data_dir, "finalscoringset")
count = 0
#rows_in_train is only used when count > 0 
rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)
#rows_in_train = c(1:100)
####Download Train data#############

CoreTable_synapse_entity <- file.path(input_data_train_dir,"CoreTable_training.csv") 
CoreTable_training <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_train_dir,"LabValue_training.csv") 
LabValue_training <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT"))

LesionMeasure_synapse_entity <- file.path(input_data_train_dir, "LesionMeasure_training.csv")
LesionMeasure_training <- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".", as.is=c("RPT"))

MedHistory_synapse_entity <- file.path(input_data_train_dir,"MedHistory_training.csv")
MedHistory_training <- read.csv(MedHistory_synapse_entity, header=T, na.strings=c(".", ""),  as.is=c("RPT"))

PriorMed_synapse_entity <- file.path(input_data_train_dir,"PriorMed_training.csv")
PriorMed_training <- read.csv(PriorMed_synapse_entity, header=T, na.strings=c(".", ""))

VitalSign_synapse_entity <- file.path(input_data_train_dir,"VitalSign_training.csv")
VitalSign_training <- read.csv(VitalSign_synapse_entity, header=T, na.strings=c(".", ""))

## Test Data


CoreTable_synapse_entity <- file.path(input_data_leaderboard_dir,"CoreTable_leaderboard.csv") 
CoreTable_test <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_leaderboard_dir,"LabValue_leaderboard.csv") 
LabValue_test <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT"))

LesionMeasure_synapse_entity <- file.path(input_data_leaderboard_dir, "LesionMeasure_leaderboard.csv")
LesionMeasure_test<- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".",as.is=c("RPT"))

MedHistory_synapse_entity <- file.path(input_data_leaderboard_dir,"MedHistory_leaderboard.csv")
MedHistory_test<- read.csv(MedHistory_synapse_entity, header=T, na.strings=c(".",""), as.is=c("RPT"))

PriorMed_synapse_entity <- file.path(input_data_leaderboard_dir,"PriorMed_leaderboard.csv")
PriorMed_test <- read.csv(PriorMed_synapse_entity, header=T, na.strings=c(".", ""))

VitalSign_synapse_entity <- file.path(input_data_leaderboard_dir,"VitalSign_leaderboard.csv")
VitalSign_test <- read.csv(VitalSign_synapse_entity, header=T, na.strings=c(".", ""))

## Split training data into training and test


## if count > 0, work only on a subset of training data
if (count > 0) {
  train_ct <- CoreTable_training[rows_in_train, ]
} else {
  train_ct <- CoreTable_training
}
train_lv <- LabValue_training[LabValue_training$RPT %in% rownames(train_ct), ]
train_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(train_ct), ]
train_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(train_ct), ]
train_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(train_ct), ]
train_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(train_ct), ]

test_ct <- CoreTable_test
test_lv <- LabValue_test[LabValue_test$RPT %in%  rownames(test_ct), ]
test_lm <- LesionMeasure_test[LesionMeasure_test$RPT %in% rownames(test_ct), ]
test_mh <- MedHistory_test[MedHistory_test$RPT %in% rownames(test_ct), ]
test_pm <- PriorMed_test[PriorMed_test$RPT %in% rownames(test_ct), ]
test_vs <- VitalSign_test [VitalSign_test$RPT %in% rownames(test_ct), ]




source("./ml_pipeline.R")
result = ml_pipeline(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
            test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)

df_predicted <- result$df_predicted
source("./score.R")


#RMSE Train
train_predictions_ttl = result$model_ttl$fit$predicted
rmse_train = score_q1b(train_predictions_ttl,CoreTable_training[names(train_predictions_ttl), c("LKADT_P")], CoreTable_training[names(train_predictions_ttl), c("DEATH")])
#RMSE Comparitive output
print(paste( "RMSE on train:",  rmse_train))

#risk train
risk_score_global <- result$risk_score_global$train 
risk_score_12 <- result$risk_score_12$train
risk_score_18 <- result$risk_score_18$train
risk_score_24 <- result$risk_score_24$train
risk_score_train <- score_q1a(CoreTable_training[names(risk_score_global), c("LKADT_P")],CoreTable_training[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])


print("Global risk score on test: ")
risk_score_global <- result$risk_score_global$test

risk_score_12 <- result$risk_score_12$test
risk_score_18 <- result$risk_score_18$test
risk_score_24 <- result$risk_score_24$test

risk_score_test <- score_q1a(df_predicted[names(risk_score_global), c("LKADT_P")],df_predicted[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])


print("-----OUTPUT---")

print("Global risk score on train: ")
risk_score_train
print("Global risk score on test: ")
risk_score_test

warnings()