source("./Utilities.R")
## main
out_dir ="./outdat"
out_dir <- setup_outdir(out_dir)

sink()
setup_log(out_dir)
flog.threshold(INFO)
set_options()

### configuration Sections
input_data_dir = "./input_dat"
input_data_train_dir = file.path(input_data_dir, "training")
input_data_leaderboard_dir = file.path(input_data_dir, "leaderboard")
input_data_final_dir = file.path(input_data_dir, "finalscoringset")
g_seeds_classifier=NULL
#g_seeds_classifier=c("./random_seeds/cleanup.discontinued_classifier_1.seed", "./random_seeds/model.discontinued_classifier_2.seed")
### End of configuration Sections


rows_in_train = NULL
#rows_in_train = c(1:200, 301:700, 801:1300,1401:1600)



source("./challenge_data.R")
train_challenge_data <- challenge_data(input_data_train_dir, "_training.csv", rows_in_train)
leaderboard_challenge_data <- challenge_data(input_data_leaderboard_dir , "_leaderboard.csv")
final_challenge_data <- challenge_data(input_data_final_dir , "_leaderboard.csv")

test_challenge_data <- rbind.challenge_data(leaderboard_challenge_data , final_challenge_data)
summary(test_challenge_data)
source("./discontinued_classifier_caret.R")
for(i in c(1:1)){
  classifier <- discontinued_classifier(train_challenge_data, test_challenge_data, 90.5, out_dir, seed_files=g_seeds_classifier)
  run_pipeline(classifier)
}
