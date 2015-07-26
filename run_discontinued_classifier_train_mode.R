source("./Utilities.R")
## main
out_dir ="./outdat_trainmode"
out_dir <- setup_outdir(out_dir)

sink()
setup_log(out_dir)
flog.threshold(INFO)
set_options()

input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
g_seeds_classifier=NULL
g_seeds_classifier=c("./random_seeds/cleanup.discontinued_classifier_1.seed", "./random_seeds/model.discontinued_classifier_2.seed")

g_seeds_risk_scorer=NULL
g_seeds_risk_scorer=c( "./random_seeds/model.discontinued_risk_scorer_1.seed", NA, "./random_seeds/cleanup.discontinued_risk_scorer_3.seed")


#rows_in_train = c(1100:1300,1401:1600)
rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)
#rows_in_train = c(1:200, 301:700, 801:1000)
rows_in_test = c(201:300,701:800,1301:1400)
#rows_in_test = c(1301:1400)
#rows_in_train = c(1401:1600)
#rows_in_test = c(201:300)

source("./challenge_data.R")
train_challenge_data <- challenge_data(input_data_train_dir, "_training.csv", rows_in_train)
test_challenge_data <- challenge_data(input_data_train_dir, "_training.csv", rows_in_test)

# source("./discontinued_classifier.R")
# classifier <- discontinued_classifier(train_challenge_data, test_challenge_data, 90.5, out_dir, seed_files=g_seeds_classifier)
# run_pipeline(classifier)
source("./discontinued_risk_scorer.R")
risk_scorer <- discontinued_risk_scorer(train_challenge_data, test_challenge_data, 90.5, out_dir, seed_files=g_seeds_risk_scorer)
run_pipeline(risk_scorer)
