cox_survival <- function (timePeriod, eventmy, regressors, data){
  library(survival)
  test1 <- list(time=c(4,3,1,1,2,2,3), 
                status=c(1,1,1,0,1,1,0), 
                x=c(0,2,1,1,1,0,0), 
                sex=c(0,0,0,0,1,1,1)) 
  # Fit a stratified model 
  fit = coxph(Surv(time, status) ~ x + strata(sex), test1) 
  
print(  coxph.detail(fit, riskmat=TRUE))
}

cox_survival()