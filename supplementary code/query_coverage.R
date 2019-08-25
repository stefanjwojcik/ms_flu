#Plotting stuff going in different zip codes::

library(zipcode)
library(ggplot2)
library(data.table)
library(lubridate)
library(gganimate)
library(ggthemes)
library(stringr)
setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/")
#setwd("/home/stefanwojcik/flu")

dat <- readRDS("zipcodeCensusData_v2.rds")
dat$zip <- as.character(dat$zip)
dat$educ.age = factor(dat$educ.age)
dat$median_inc = scale(dat$median_inc)

# GET THE QUERIES DATA:
qdat <- read.csv("AllStates_Zipcode_fludata_nobadlines.csv", stringsAsFactors = F)
names(qdat) = c("V1", "QueryDate", "WeekID", "SessionID", "NormalizedQuery", "finalCode", "PostalCode", "Gender", "AgeInYears", "State")
qdat$date <- as.Date(qdat$QueryDate)
qdat$zip <- as.character(qdat$PostalCode)
qdat$A1 <- 1*(qdat$finalCode=="a1") #|qdat$finalCode=="b1" if we want to include secondary symptoms
qdat$month = factor(month(qdat$date), ordered = T)
qdat$year = year(qdat$date)

# How many of this zip codes are represented in the the ACS set?
qdat = qdat[tolower(qdat$State) %in% tolower(c(state.name, "District Of Columbia")),  ]
qdat = qdat[nchar(qdat$zip)==5, ]
# remove those that are missing from the source zip file
qdat = qdat[qdat$zip %in% dat$zip, ]

# MERGE CENSUS FEATURES WITH THE QUERY DATA
qdat <- merge(qdat, dat, by="zip")

# VOLUME OF A1 and Non-A1 (those that contained a related doc2vec term)
nrow(qdat)

# TEMPORAL SCALE
summary(qdat$date)

# GEOGRAPHIC COVERAGE
# all 50 states plus the district of Columbia
sum(unique(tolower(qdat$State)) %in% tolower(c(state.name, "District Of Columbia")))

sum(qdat$zip %in% dat$zip)/nrow(qdat)
# nearly 3695 missing zip codes missing from ACS census data - not sure why this is
length(unique(qdat$zip[!qdat$zip %in% dat$zip]))

# the proportion of zip codes included
sum(unique(qdat$zip) %in% dat$zip)/nrow(dat)
# which zips are represented?
states_plotted = table(qdat$state)
norm_queries = qdat %>%
  select(State, pop, year.x, A1) %>%
  group_by(State, year.x) %>%
  summarise(qcount = n()/mean(pop), A1count = sum(A1)/mean(pop)) %>%
  group_by(State) %>%
  summarise(qcount = mean(qcount), A1count = mean(A1count)) %>%
  mutate(Query_count = (qcount-mean(qcount))/sd(qcount), A1_count = (A1count - mean(A1count))/sd(A1count)) %>%
  select(State, Query_count, A1_count)
# Factor according to size of state counts
norm_queries$State = factor(norm_queries$State, levels = unique(norm_queries$State[order(norm_queries$Query_count)]))

# Reshape to Long
library(reshape2)
d <- melt(norm_queries, id.vars="State")

# PLOT the data as a dot plot
ggplot(d, aes(y = State, x = value, col = variable, fill=variable)) +  
  geom_point() + xlab("Avg Annual Query Counts Normalized by State Population, Converted to Z-score (2012-2017)")

###### ALL QUERIES

setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/")
library(noncensus)
library(data.table)
library(lubridate)
# Importing data from the 'noncensus' package
data(states) 
states = states[, c("name","population")]
states$name = gsub(" ", ".", states$name)

# ALL QUERIES
all_qs = read.csv("SRPV_AllStates.csv", stringsAsFactors = F)
all_qs$Year = str_sub(all_qs$WeekID, -4)
all_qs$WeekID = NULL
all_qs = melt(all_qs, id.vars = c("Year"))
all_qs$variable = gsub("\\.", " ", all_qs$variable)
all_qs = all_qs %>%
  

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
