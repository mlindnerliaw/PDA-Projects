library(knitr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(kableExtra)
library(gt)


## Get year ranges for each race
#####################################################
## numbers for summary in text
get_years<-function(data, race){
  #' this function finds the range of years for a given race in the data
  #' @param data, the dataframe containing at least the race and year columns
  #' @param race, character name of race
  #' @return vector containing the start and endpoints of the range of years for the given race
  #'
  data%>%filter(Race==race)%>%select(Year)%>%unique()%>%range()
}

# boston race years
get_years(full_dat, "Boston")
# NYC race years
get_years(full_dat, "New York City")
# TC race years
get_years(full_dat, "Twin Cities")
# Gma's race years
get_years(full_dat, "Grandma's")
# Chi race years
get_years(full_dat, "Chicago")
