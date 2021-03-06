# Dream challenge prostate cancer

## Pre requisites
1. Install R 3.1.0 (Make sure this exact version of R is installed for automatic package restore from packrat to work)
2. Download the training, leaderboard & final scoring set data from https://www.synapse.org/#!Synapse:syn3325825

## Set up
1. Start up an R session and set up the working directory to this projects root.
2. To install all packages used by this project, in R terminal run the following commands
    - packrat::init()
    - packrat::restore()
3. This scripts expects input train and test file names exactly the same convention as defined in the challenge data from synapse.


## Run sub challenge 1a -  Relative risk score for patients.

To run in test mode to predict scores for the test or leaderboard data

run_survivalanalysisriskscore_test_mode.R inputtrainingdatadir filesuffix inputfinalscoredatadir filesuffix outdir

 e.g
 
 `R --no-save --args ./training_data  _training.csv ./FinalScoringSet _FinalScoringSet.csv  outdir < run_survivalanalysisriskscore_test_mode.R`
   
- All errors would be logged in the runall.log file, found in the output directory specified in the argument
- If the run is successful, the submission file submission1a.csv, can be found in the subfolder survivalanalysis_risk_scorer... within the output directory



## Run sub challenge 1b -  Time to event for patients.

To run in test mode to predict time to event for the test or leaderboard data

run_timetodeath_predictor_test_mode.R inputtrainingdatadir filesuffix inputfinalscoredatadir filesuffix outdir

 e.g
 
 `R --no-save --args ./training_data  _training.csv ./FinalScoringSet _FinalScoringSet.csv  outdir < run_timetodeath_predictor_test_mode.R`
   
- All errors would be logged in the runall.log file, found in the output directory specified in the argument
- If the run is successful, the submission file submission1b.csv, can be found in the subfolder with prefix daystodeath_predictor... within the output directory 






