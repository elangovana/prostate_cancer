options(echo=F)
source("./Utilities.R")
## main
args_default <- c( "./input_dat/training" , "_training.csv", "./input_dat/finalscoringset", "_leaderboard.csv", "./output_data")
args<-commandArgs(trailingOnly = TRUE)

if (length(args) != 5) {
    args <- args_default
    print("No arguments supplied or number of arguments is not 5. Running with defaults")
    print("To alter default, Usage :")
    print("run_timetodeath_predictor_test_mode.r inputtrainingdatadir filesuffix inputfinalscoredatadir filesuffix outdir")
}
print("Using arguments:")
print(args)

out_dir = args[5]
out_dir <- setup_outdir(out_dir)

sink()
setup_log(out_dir)
flog.threshold(INFO)
set_options()



#g_seed_files = NULL
g_seed_files = c("./random_seeds/model_daystodeath_predictor_1.seed","./random_seeds/model_daystodeath_predictor_2.seed","./random_seeds/model_daystodeath_predictor_3.seed", "./random_seeds/model_daystodeath_predictor_4.seed", "./random_seeds/model_daystodeath_predictor_5.seed")

input_data_train_dir = args[1]
g_train_files_suffix = args[2]
input_data_test_dir = args [3]
g_test_files_suffix = args[4]

#rows_in_train = c(1100:1300,1401:1600)
#rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)
rows_in_train = NULL
#rows_in_train = c(1:200, 301:700, 801:1000)
rows_in_test = NULL
#rows_in_test = c(1301:1400)
#rows_in_train = c(1401:1600)
#rows_in_test = c(201:300)

source("./challenge_data.R")
train_challenge_data <- challenge_data(input_data_train_dir, g_train_files_suffix, rows_in_train)
test_challenge_data <- challenge_data(input_data_test_dir, g_test_files_suffix, rows_in_test)

source("./daystodeath_predictor.R")
predictor <- daystodeath_predictor(train_challenge_data, test_challenge_data,  out_dir, seed_files=g_seed_files)
run_pipeline(predictor)
