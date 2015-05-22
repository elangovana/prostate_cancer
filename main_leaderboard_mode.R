##########
input_data_dir = "./input_dat"
out_dir = "./out_dat"
input_data_train_dir = file.path(input_data_dir, "training")
input_data_leaderboard_dir = file.path(input_data_dir, "leaderboard")
count = 50
####Download Train data#############

CoreTable_synapse_entity <- file.path(input_data_train_dir,"CoreTable_training.csv") 
CoreTable_training <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP", "DOMAIN", "STUDY"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_train_dir,"LabValue_training.csv") 
LabValue_training <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT","DOMAIN", "STUDY"))

LesionMeasure_synapse_entity <- file.path(input_data_train_dir, "LesionMeasure_training.csv")
LesionMeasure_training <- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".")

MedHistory_synapse_entity <- file.path(input_data_train_dir,"MedHistory_training.csv")
MedHistory_training <- read.csv(MedHistory_synapse_entity, header=T, na.strings=".")

PriorMed_synapse_entity <- file.path(input_data_train_dir,"PriorMed_training.csv")
PriorMed_training <- read.csv(PriorMed_synapse_entity, header=T, na.strings=".")

VitalSign_synapse_entity <- file.path(input_data_train_dir,"VitalSign_training.csv")
VitalSign_training <- read.csv(VitalSign_synapse_entity, header=T, na.strings=".")

## Test Data


CoreTable_synapse_entity <- file.path(input_data_leaderboard_dir,"CoreTable_leaderboard.csv") 
CoreTable_test <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP", "DOMAIN", "STUDY"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_leaderboard_dir,"LabValue_leaderboard.csv") 
LabValue_test <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT","DOMAIN", "STUDY"))

LesionMeasure_synapse_entity <- file.path(input_data_leaderboard_dir, "LesionMeasure_leaderboard.csv")
LesionMeasure_test<- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".")

MedHistory_synapse_entity <- file.path(input_data_leaderboard_dir,"MedHistory_leaderboard.csv")
MedHistory_test<- read.csv(MedHistory_synapse_entity, header=T, na.strings=".")

PriorMed_synapse_entity <- file.path(input_data_leaderboard_dir,"PriorMed_leaderboard.csv")
PriorMed_test <- read.csv(PriorMed_synapse_entity, header=T, na.strings=".")

VitalSign_synapse_entity <- file.path(input_data_leaderboard_dir,"VitalSign_leaderboard.csv")
VitalSign_test <- read.csv(VitalSign_synapse_entity, header=T, na.strings=".")

## Split training data into training and test


## if count > 0, work only on a subset of training data
if (count > 0) {
  train_ct <- CoreTable_training[c(1:count), ]
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
ml_pipeline(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
            test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)

