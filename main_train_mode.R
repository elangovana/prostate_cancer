##########
input_data_dir = "./input_dat"
out_dir ="./outdat_trainmode"
input_data_train_dir = file.path(input_data_dir, "training")
count = 10
test_count = 3

####Download data#############

CoreTable_synapse_entity <- file.path(input_data_train_dir,"CoreTable_training.csv") 
CoreTable_training <- read.csv(CoreTable_synapse_entity, row.names="RPT", header=T, na.strings=c(".",""), as.is=c("RPT","AGEGRP", "DOMAIN", "STUDY"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_train_dir,"LabValue_training.csv") 
LabValue_training <- read.csv(LabValue_synapse_entity, header=T, na.strings=".",as.is=c("RPT", "DOMAIN", "STUDY"))

LesionMeasure_synapse_entity <- file.path(input_data_train_dir, "LesionMeasure_training.csv")
LesionMeasure_training <- read.csv(LesionMeasure_synapse_entity, header=T, na.strings=".")

MedHistory_synapse_entity <- file.path(input_data_train_dir,"MedHistory_training.csv")
MedHistory_training <- read.csv(MedHistory_synapse_entity, header=T, na.strings=".")

PriorMed_synapse_entity <- file.path(input_data_train_dir,"PriorMed_training.csv")
PriorMed_training <- read.csv(PriorMed_synapse_entity, header=T, na.strings=".")

VitalSign_synapse_entity <- file.path(input_data_train_dir,"VitalSign_training.csv")
VitalSign_training <- read.csv(VitalSign_synapse_entity, header=T, na.strings=".")

##

## Split training data into training and test
train_ct <- CoreTable_training[c(1:count), ]
train_lv <- LabValue_training[LabValue_training$RPT %in% rownames(train_ct), ]
train_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(train_ct), ]
train_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(train_ct), ]
train_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(train_ct), ]
train_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(train_ct), ]

test_ct <- CoreTable_training[c(count+1:test_count), ]
test_lv <- LabValue_training[LabValue_training$RPT %in%  rownames(test_ct), ]
test_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% rownames(test_ct), ]
test_mh <- MedHistory_training[MedHistory_training$RPT %in% rownames(test_ct), ]
test_pm <- PriorMed_training [PriorMed_training $RPT %in% rownames(test_ct), ]
test_vs <- VitalSign_training [VitalSign_training$RPT %in% rownames(test_ct), ]



source("./ml_pipeline.R")
df_predicted <- ml_pipeline(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
  test_ct, test_lv, test_lm, test_mh, test_pm, test_vs, out_dir)

head(df_predicted)
str(df_predicted)
write.csv(merge(test_ct, df_predicted, by=0, all=TRUE, suffixes= c("ct", "pred" )), file=file.path(out_dir, "trainmode_test_actual_vsp_red.csv"))
