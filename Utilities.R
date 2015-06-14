plot_survival <- function(time, events){
  library(survival)
  
  kaplan_meier <- survfit(Surv(time, events) ~ 1)
  #print(summary(kaplan_meier))
  #plot(kaplan_meier)
}