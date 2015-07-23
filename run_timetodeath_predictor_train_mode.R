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
g_seed_files = NULL
#g_seed_files = c("./random_seeds/model_daystodeath_predictor_1.seed","./random_seeds/model_daystodeath_predictor_2.seed", "./random_seeds/model_daystodeath_predictor_4.seed")


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

source("./daystodeath_predictor.R")
predictor <- daystodeath_predictor(train_challenge_data, test_challenge_data,  out_dir, seed_files=g_seed_files)
run_pipeline(predictor)
