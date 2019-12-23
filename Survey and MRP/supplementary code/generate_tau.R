# SETTING THE TAU
setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/")
library(noncensus)
library(data.table)
library(lubridate)
# Importing data from the 'noncensus' package
data(states) 
states = states[, c("name","population")]
states$name = gsub(" ", ".", states$name)

# DENOMINATOR
A1denom = read.csv("SRPV_AllStates.csv", stringsAsFactors = F)
A1denom = A1denom[grepl("2014|2015", A1denom$WeekID), ]
A1denom$week = as.numeric(substr(A1denom$WeekID, start=2, stop=nchar(A1denom$WeekID)-4))
A1denom$year = as.numeric(substr(A1denom$WeekID, start=nchar(A1denom$WeekID)-3, stop=nchar(A1denom$WeekID)))
# Isolate the typical flu season - last 13 weeks of the year and beginning 20 weeks
weeks2014 = A1denom$year==2014 & A1denom$week>=40
weeks2015 = A1denom$year==2015 & A1denom$week<=20
A1denom = A1denom[ weeks2014|weeks2015, ]

# get the average over the flu season by averaging all columns
A1denom = apply(A1denom[, 2:51], 2, function(x) sum(x, na.rm=T))
A1denom = data.frame(name=names(A1denom), total_searches = A1denom)
A1denom = merge(A1denom, states, by="name", all.x = T)
A1denom$pct = as.numeric(A1denom$population)/sum(as.numeric(A1denom$population))
denom = weighted.mean(A1denom$total_searches, w=A1denom$pct)

# NUMERATOR
# GET THE QUERIES DATA:
qdat <- fread("allFluData_nobadlines.csv", sep=",")
names(qdat) = c("V1", "QueryDate", "WeekID", "SessionID", "NormalizedQuery", "finalCode", "PostalCode", "Gender", "AgeInYears", "State")
# How many of this zip codes are represented in the the ACS set?
qdat = qdat[tolower(qdat$State) %in% tolower(c(state.name, "District Of Columbia", "Puerto Rico")),  ]
qdat$date <- as.Date(qdat$QueryDate)
qdat$zip <- as.character(qdat$PostalCode)
qdat = qdat[nchar(qdat$zip)==5, ]
qdat$A1 <- 1*(qdat$finalCode=="a1") #|qdat$finalCode=="b1" if we want to include secondary symptoms
qdat = qdat[ qdat$date >= "2014-10-02" & qdat$date < "2015-05-22", ]
qdat = aggregate(A1~State, sum, data=qdat)
qdat$State = gsub(" ", ".", qdat$State)
qdat = merge(qdat, states, by.x="State", by.y="name")
numer = weighted.mean(qdat$A1, w=qdat$population)

rm(A1denom, qdat, states, weeks2014, weeks2015)


# Create tau file
tau = data.frame(denom = denom, numer = numer, tau = numer/denom)
saveRDS(tau, "~/Documents/ms_flu/data/tau.rds")
