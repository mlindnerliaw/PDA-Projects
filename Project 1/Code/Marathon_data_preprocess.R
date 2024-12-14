library(knitr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(kableExtra)
library(gt)

# read course record data
cr_dat<-read.csv("course_record.csv")
# expand race names
cr_dat$Race<-case_when(cr_dat$Race=="B"~"Boston",
                       cr_dat$Race=="C"~"Chicago",
                       cr_dat$Race=="D"~"Grandma's",
                       cr_dat$Race=="NY"~"New York City",
                       TRUE~"Twin Cities")

# read data
dat<-read.csv("project1.csv")
# rename columns
names(dat)<-c("Race", "Year", "Sex", "Flag", "Age", "Percent_CR", "TD", "TW", 
              "RH", "TG", "SR", "DP", "Wind", "WBGT")
dat$Sex<-ifelse(dat$Sex==1, "M", "F")
# label races by location
dat$Race<-case_when(dat$Race==0~"Boston",
                    dat$Race==1~"Chicago",
                    dat$Race==2~"New York City",
                    dat$Race==3~"Twin Cities",
                    TRUE~"Grandma's")



# join course record data by race, year, and gender
full_dat<-left_join(dat, cr_dat, by=c("Race", "Year", "Sex"="Gender"))

# convert CR to minutes
in_minutes<-function(times){
  #' takes a vector of times in the format "hours:minutes:seconds" and returns the number of minutes
  #' @param times a character vector containing the times to convert
  #' @return a numeric vector of the same length as times containing the times in minutes
  #'
  
  # convert to hours minutes and seconds
  convert<-hms(times)
  
  # calculate minutes
  mins<-hour(convert)*60+minute(convert)+second(convert)/60
  return(mins)
}

#convert percent_cr to times
full_dat<-full_dat%>%mutate(cr.time=in_minutes(CR), 
                            Time=cr.time*(1+Percent_CR/100))



# fix irregularity of decimal relative humidities by converting to percent
full_dat<-full_dat%>%mutate(RH=ifelse(RH<1, RH*100, RH))

# blank flag values are black missing
full_dat$Flag<-ifelse(full_dat$Flag=="", NA, full_dat$Flag)
