library(knitr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(kableExtra)
library(gt)




## create table 1
###################################################################################
tbl_summary(select(full_dat, -c(Year, CR, cr.time)), by=Race, missing="no",
            statistic = list(all_continuous()~ "{mean} ({sd})",
                             all_categorical()~ "{n} ({p}%)"))%>%
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}")%>%
  modify_caption("Data Characteristics by Race")%>%
  as_kable_extra(booktabs=TRUE)




## Create Figure 1
#######################################################################################
#### plot percent course record with smoothed line, by sex and race
g<-ggplot(full_dat, aes(x=Age, y=Percent_CR, color=Sex))+
  geom_point(alpha=.7)+
  geom_smooth(aes(x=Age, y=Percent_CR, color="orange"), dat=full_dat[full_dat$Sex=="M",])+
  geom_smooth(aes(x=Age, y=Percent_CR, color="lightblue"), dat=full_dat[full_dat$Sex=="F",])+
  #facet_wrap(~Race, )+
  scale_color_manual(values=c("blue","lightblue", "tomato", "orange"), labels=c("Female", "Female", "Male", "Male"))+
  labs(x="Age", y="Percent CR", title=
         "Figure 1: Finishing Times as Percent Course Record by Sex and Race")+
  theme(legend.position = "top",
        panel.background = element_rect(fill="white"),
        strip.background =element_rect(fill="grey90", linewidth = .2,
                                       colour = "grey70"),
        panel.grid.major = element_line(color="grey70", linewidth=.2),
        panel.grid.minor = element_line(color="grey70", linewidth=.2),
        legend.key = element_rect(fill = "grey90")
  )

g



## Create Figure 2
#############################################################################################
# create bins for age
full_dat<-full_dat%>%mutate(Age_cat=case_when(Age<20~"<20",
                                              Age<50~"20-49",
                                              Age<75~"50-74",
                                              TRUE~"75+"))

### plot times by race and sex
ggplot(full_dat, aes(x=Percent_CR, fill=Sex))+
  geom_density(alpha=.75)+
  facet_wrap(~Age_cat)+
  scale_fill_manual(values=c("blue","tomato"), labels=c("Female", "Male"))+
  labs(x="Percent CR", y="Density", title="Figure 2: Distribution of Finishing Times by Sex and Age Category")+
  theme(legend.position = "top",
        panel.background = element_rect(fill="white"),
        strip.background =element_rect(fill="grey90", linewidth = .2,
                                       colour = "grey70"),
        panel.grid.major = element_line(color="grey70", linewidth=.2),
        panel.grid.minor = element_line(color="grey70", linewidth=.2),
        legend.key = element_rect(fill = "grey90")
  )





## Create Table 2
######################################################################################
## table of data characteristics by flag
tbl_summary(select(full_dat, c(Race, Sex, Age, Percent_CR,Time, Flag)), by=Flag, 
            missing="no",
            statistic = list(all_continuous()~ "{mean} ({sd})",
                             all_categorical()~ "{n} ({p}%)"))%>%
  add_p()%>%
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}")%>%
  modify_caption("Data Characteristics by Flag")%>%
  as_kable_extra(booktabs=TRUE)







## Create Figure 3
####################################################################################
ggplot(full_dat[!is.na(full_dat$Flag),], aes(x=Percent_CR, fill=Sex))+
  geom_density(alpha=.75)+
  facet_wrap(~Flag)+
  scale_fill_manual(values=c("blue", "tomato"), labels=c("Female", "Male"))+
  labs(x="Percent_CR", y="Percent CR", title=
         "Figure 3: Distribution of Finishing Times by Sex and Flag")+
  theme(legend.position = "top",
        panel.background = element_rect(fill="white"),
        strip.background =element_rect(fill="grey90", linewidth = .2,
                                       colour = "grey70"),
        panel.grid.major = element_line(color="grey70", linewidth=.2),
        panel.grid.minor = element_line(color="grey70", linewidth=.2),
        legend.key = element_rect(fill = "grey90")
  )


## Create Figure 4
####################################################################################
ggplot(full_dat[!is.na(full_dat$Flag),], aes(x=Percent_CR, color=Flag))+
  geom_density(alpha=.75, linewidth=.75)+
  facet_wrap(~Age_cat)+
  scale_color_manual(values=c("green", "red","black", "gold"), 
                     labels=c("Green", "Red", "White", "Yellow"))+
  labs(x="Percent_CR", y="Density", title=
         "Figure 4: Distribution of Finishing Times by Age and Flag")+
  theme(legend.position = "top",
        panel.background = element_rect(fill="white"),
        strip.background =element_rect(fill="grey90", linewidth = .2,
                                       colour = "grey70"),
        panel.grid.major = element_line(color="grey70", linewidth=.2),
        panel.grid.minor = element_line(color="grey70", linewidth=.2),
        legend.key = element_rect(fill = "grey90")
  )



## Create Table 3
####################################################################################
# linear regression of time with WBGT interactions
# set baseline age group to 20-49
full_dat$Age_cat<-factor(full_dat$Age_cat, ordered = FALSE)
full_dat$Age_cat<-relevel(full_dat$Age_cat, ref="20-49")

l<-lm(Time~Age_cat*WBGT+Sex*WBGT, data=full_dat)

# summary table of regression coefficients
tbl_regression(l)%>%
  modify_caption("Linear Regression of Time (m) with WBGT Interactions")%>%
  as_kable_extra(booktabs=TRUE)





## Create Table 4
###################################################################################
# fit linear model for weather conditions
weather_fit<-lm(Time~RH+SR+DP+Wind+WBGT, data=full_dat)

# summary table of regression coefficients
tbl_regression(weather_fit)%>%
  modify_caption("Linear Regression of Time (m) and Weather Conditions")%>%
  as_kable_extra(booktabs=TRUE)
