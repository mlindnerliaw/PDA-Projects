library(knitr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(kableExtra)
library(gt)


## Missing data checking
###############################################################################
num_missing<-function(data){
  #' Returns the number of missing values in the given data
  #' @param data, data vector or data frame of any type
  #' @return the number of NA values in the data
  
  return(length(which(is.na(data))))
}

# table of missingness proportion
kable(apply(full_dat, 2, num_missing)/nrow(dat))

# look at races with missingness, choosing arbitrary age present in all races
# and first weather measurement
full_dat%>%filter(is.na(TD))%>%group_by(Race)%>%filter(Age==25)
