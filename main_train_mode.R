##########


#set up logging
setup_log <- function(outdir){
  con <- file(file.path(outdir,"runall.log"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
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

sink()
out_dir <- setup_outdir(out_dir)
setup_log(out_dir)

input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
count = 500
test_count = 100




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
PriorMed_training <- read.csv(PriorMed_synapse_entity, header=T, na.strings=".", as.is=c("RPT"))

VitalSign_synapse_entity <- file.path(input_data_train_dir,"VitalSign_training.csv")
VitalSign_training <- read.csv(VitalSign_synapse_entity, header=T, na.strings=".", as.is=c("RPT"))

##

## Split training data into training and test
train_ct <- CoreTable_training[c(1:count), ]
train_lv <- LabValue_training[LabValue_training$RPT %in% rownames(train_ct), ]
train_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(train_ct), ]
train_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(train_ct), ]
train_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(train_ct), ]
train_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(train_ct), ]

test_ct <- CoreTable_training[c(count+1:test_count), !colnames(CoreTable_training) %in% c("LKADT_P", "DEATH", "DISCONT",  "ENDTRS_C",  "ENTRT_PC") ]
test_lv <- LabValue_training[LabValue_training$RPT %in%  rownames(test_ct), ]
test_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(test_ct), ]
test_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(test_ct), ]
test_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(test_ct), ]
test_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(test_ct), ]



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

#risk train
risk_score_global <- result$risk_score_global$train$fit
risk_score_12 <- result$risk_score_12$train$fit
risk_score_18 <- result$risk_score_18$train$fit
risk_score_24 <- result$risk_score_24$train$fit
risk_score_train <- score_q1a(CoreTable_training[names(risk_score_global), c("LKADT_P")],CoreTable_training[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])


print("Global risk score on test: ")
risk_score_global <- result$risk_score_global$test$fit
risk_score_12 <- result$risk_score_12$test$fit
risk_score_18 <- result$risk_score_18$test$fit
risk_score_24 <- result$risk_score_24$test$fit
risk_score_test <- score_q1a(df_predicted[names(risk_score_global), c("LKADT_P")],df_predicted[names(risk_score_global), c("DEATH")], risk_score_global, risk_score_12[names(risk_score_global)], risk_score_18[names(risk_score_global)], risk_score_24[names(risk_score_global)])

print("-----OUTPUT---")
#RMSE Comparitive output
print(paste("RMSE on test: ", rmse_test, "RMSE on train:",  rmse_train))
print("Global risk score on train: ")
risk_score_train
print("Global risk score on test: ")
risk_score_test

warnings()