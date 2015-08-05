# Dream challenge prostate cancer

## Pre requisites
1. Install R
2. Download the training, leaderboard & final scoring set data from https://www.synapse.org/#!Synapse:syn3325825

## Set up
1. Start up an R session and set up the working directory to this projects root.
2. To install all packages used by this project, in R terminal run the following command

    a) packrat::init()
    
    b) packrat::restore()

## How to run
This scripts expects input train and test file names exactly the same convention as defined in the challenge data from synapse.

### Run sub challenge 1a -  Relative risk score for patients.

1. To run in test mode to predict scores for the test or leaderboard data
Specify the input directory
run_survivalanalysisriskscore_test_mode.R <inputtrainingdatadir> <filesuffix> <inputfinalscoredatadir> <filesuffix> <outdir>




