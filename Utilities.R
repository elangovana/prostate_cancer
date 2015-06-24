plot_survival <- function(time, events){
  library(survival)
  
  kaplan_meier <- survfit(Surv(time, events) ~ 1)
  #print(summary(kaplan_meier))
  #plot(kaplan_meier)
}

calc_dummy_score <- function(row, days, lowerdays=0){
  #   # return(as.numeric(row["LKADT_P"]))
  #    if (is.na(row["DEATH"]))  {return(as.numeric(row["LKADT_P"]))}
  #    if (as.character(row["DEATH"]) =="YES") {return(as.numeric(row["LKADT_P"] )+ 50000)}
  #    return(as.numeric(row["LKADT_P"]))
  
  result = 0
  if(is.na(row["DEATH"])){
    result = 0
  }
  else if (as.character(row["DEATH"]) =="YES"){
    if((as.numeric(row["LKADT_P"]) >= lowerdays) & (as.numeric(row["LKADT_P"]) < days)) {result = 1/as.numeric(row["LKADT_P"])}
    else{result = -1 * as.numeric(row["LKADT_P"])}
  }
  return(result)
}


source("./score.R")
