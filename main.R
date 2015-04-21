##########
input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
count = 2
test_count = count + 3

####Download data#############

CoreTable_synapse_entity <- file.path(input_data_train_dir,"CoreTable_training.csv") 
CoreTable_training <- read.csv(CoreTable_synapse_entity, header=T, na.strings=".", as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- file.path(input_data_train_dir,"LabValue_training.csv") 
LabValue_training <- read.csv(LabValue_synapse_entity, header=T, na.strings=".")

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
train_lv <- LabValue_training[LabValue_training$RPT %in% train_ct$RPT, ]
train_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% train_ct$RPT, ]
train_mh <- MedHistory_training[MedHistory_training$RPT %in% train_ct$RPT, ]
train_pm <- PriorMed_training [PriorMed_training $RPT %in% train_ct$RPT, ]
train_vs <- VitalSign_training [VitalSign_training$RPT %in% train_ct$RPT, ]


test_ct <- CoreTable_training[c(count+1:test_count), ]
test_lv <- LabValue_training[LabValue_training$RPT %in% test_ct$RPT, ]
test_lm <- LesionMeasure_training[LesionMeasure_training$RPT %in% test_ct$RPT, ]
test_mh <- MedHistory_training[MedHistory_training$RPT %in% test_ct$RPT, ]
test_pm <- PriorMed_training [PriorMed_training $RPT %in% test_ct$RPT, ]
test_vs <- VitalSign_training [VitalSign_training$RPT %in% test_ct$RPT, ]

source("./alg_random_forest.R")
alg_random_forest(train_ct, train_lv, train_lm, train_mh, train_pm, train_vs,
test_ct, test_lv, test_lm, test_mh, test_pm, test_vs)