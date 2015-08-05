# Dream challenge prostate cancer

## Pre requisites
1. Install R
2. Install synapseClient as detailed in https://sagebionetworks.jira.com/wiki/display/SYNR/How+to+install+the+Synapse+R+Client

## Set up
1. Start up an R session and set up the working directory to this projects root.
2. To install all packages used by this project, in R terminal run the following command

    a) packrat::init()
    
    b) packrat::restore()

## How to run
### Run sub challenge 1a -  Relative risk score for patients.

1. To run in test mode to predict scores for the test or leaderboard data

run_survivalanalysisriskscore_test_mode.R <inputtraindatadir> <inputtestdatadir> <outputdir> 




