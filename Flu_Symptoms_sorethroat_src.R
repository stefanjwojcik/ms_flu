#Report on Flu Project
#Stefan Wojcik
# Results originally produce for a report on 9/22/2015
# This file will simply create the appropriate data file for analysis

#Load libraries
library(foreign)
library(ggplot2)
library(stargazer)
library(effects)

#Load datasets
setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data")
dat <- read.csv("MS556_All_Data_051515_A1Codes_w_sorethroat.csv", na.strings=c("#NULL!", NA))

# marking those who responded in the earlier- pre- April 15 wave, compared to those that responded after
dat$early_response = as.Date(as.character(dat$starttime), format = "%m/%d/%Y %M:%S") > "2015-04-15"

#Reliability - see reliability_final_codings_09182015.R

#Flu incidence and flu-like search basic means
anyflu <- rowSums(dat[, grepl("_q|_p", names(dat))]) #Any type of page or query that was triggered

#Trigger <-  1*(rowSums(dat[, grepl("_q|_p", names(dat))])!=0) #Numeric version for correlation table
Trigger <-  ifelse(rowSums(dat[, grepl("_q|_p", names(dat))])==0, "No Trigger", "Trigger") 

a1 <- 1*(rowSums(dat[, grepl("A1_q|A1_p", names(dat))])>0) #Searched for flu
a2 <- 1*(rowSums(dat[, grepl("A2_q|A2_p", names(dat))])>0) #Searched for flu
b1 <- 1*(rowSums(dat[, grepl("B1_q|B1_p", names(dat))])>0) #Searched for flu
b2 <- 1*(rowSums(dat[, grepl("B2_q|B2_p", names(dat))])>0) #Searched for flu

#Only User of Machine and primary searcher
prm.user <- dat$QS6_6_M==1|dat$QS8x1_N>=80
prm.searcher <- dat$QH4_S==1

#Gender
female <- 1*(dat[, grepl("QS2_S", names(dat))]==2) #Women
#Parent - those with children in the home that are under 18. Those 0-10 or 11-17. 
parent <- 1*(apply(dat[, grepl("QH2x1_N|QH2x2_N", names(dat))], 1, function(x) sum(x, na.rm=T)>0) ) 

#Count number of children 
pyoungchild <- dat[, c("QH2x1_N")]>0

#Flu Volume
volume <- log(dat$QVOLUMN)

#Flu Symptoms - Household flu

#Spouse
spouse <- 1*(dat[, grepl("QS4_S", names(dat))]==1)

#Respondent Symptoms - different conjuctive symptoms
r.symp <- dat[, grepl("QIR1_", names(dat))] #isolate the flu symptoms
#Fever only
r.fever <- r.symp[,1]
#Sick with cough
r.cough <-  r.symp[, 2]
#Sick with sore throat
r.sorethroat <- r.symp[, 3]
#Full Flu
r.flu <- 1*(r.fever*r.cough|r.fever*r.sorethroat)
#Spouse Symptoms
s.symp <- dat[, grepl("QIR1b_[0-9]_M|QIR1b_[0-9]{2}_M", names(dat))] #isolate the flu symptoms
#spouse with Fever only
s.fever <- s.symp[,1]
#Spouse with cough
s.cough <-  s.symp[, 2]
#Spouse with sore throat
s.sorethroat <- s.symp[, 3]
#spouse with Flu
s.flu <- 1*(s.fever*s.cough|s.fever*s.sorethroat)
#Child Symptoms
ch.symp <- dat[, grepl("QIR1a", names(dat))] #isolate the flu symptoms
#Fever only
ch.fever <- ch.symp[,1]
#Sick with cough
ch.cough <-  ch.symp[, 2]
#Child with Sore Throat
ch.sorethroat <- ch.symp[, 3]
#Flu
ch.flu <- 1*((ch.fever*ch.cough|ch.fever*ch.sorethroat)>0)
#ch.flu <- factor((ch.fever*ch.cough)>0, labels=c("no", "yes"))

# alternative based on: https://www.influenzanet.eu/en/flu-activity/ (look at sidebar)
Household.Flu <- 1*(rowSums(data.frame(r.fever*r.cough|r.fever*r.sorethroat, 
                                       s.fever*s.cough|s.fever*s.sorethroat, 
                                       ch.fever*ch.cough|ch.fever*ch.sorethroat), na.rm=T)>0)
#Household.Flu <- factor(rowSums(data.frame(r.fever*r.cough, s.fever*s.cough, ch.fever*ch.cough), na.rm=T)>0, labels=c("no", "yes"))
# Add in other demographics as well:
education = dat[, grepl("QSEN1_S", names(dat))]
education = factor(education, labels = c("No HS", "HS", 
                                         "Some College", "Assoc. Degree", 
                                         "Bach. Degree", "Grad. Degree"))
# REORDERING THE LEVELS
education = factor(education, levels = c( "HS", "No HS",
                                          "Some College", "Assoc. Degree", 
                                          "Bach. Degree", "Grad. Degree"))

race = dat[, grepl("QSEN2_S", names(dat))]
race = factor(race, labels = c("White", "Black", 
                               "Native", "Asian-Pacific", 
                               "Hispanic", "Other", "DK"))

#Data for correlation table
d1 <- data.frame(A1=a1, A2=a2, B1=b1, B2=b2, Any.Flu=anyflu, Volume=volume, Trigger, Female=female, 
                 Parent=parent, Spouse=spouse, Age=dat$QAGE, Household.Flu, r.flu, s.flu, ch.flu, 
                 prm.user, education = education, race = race)
names(d1) = tolower(names(d1))

rm(list=setdiff(ls(), "d1"))

