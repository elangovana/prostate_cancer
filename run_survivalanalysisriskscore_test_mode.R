source("./Utilities.R")
## main
out_dir ="./outdat"
out_dir <- setup_outdir(out_dir)

sink()
setup_log(out_dir)
flog.threshold(INFO)
set_options()

input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
input_data_test_dir = file.path(input_data_dir, "leaderboard")

#rows_in_train = c(1401:1600)
#rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)
rows_in_train=NULL

source("./challenge_data.R")
train_challenge_data <- challenge_data(input_data_train_dir, "_training.csv", rows_in_train)
test_challenge_data <- challenge_data(input_data_test_dir , "_leaderboard.csv")


source("./survivalanalysis_risk_scorer.R")
predictor <- survivalanalysis_risk_scorer(train_challenge_data, test_challenge_data,  out_dir)
run_pipeline(predictor)
