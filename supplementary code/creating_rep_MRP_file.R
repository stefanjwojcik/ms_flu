#processing flu query data to create a valid demo file for reproducibility
###################################################
# PROCESSING THE DATA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(lme4) # version on achtung server is lme4_1.1-12
library(doMC)
library(caret) 
library(parallel)
library(reshape)
library(data.table)
library(lubridate)
#registerDoMC(6)
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
sum(qdat$zip %in% dat$zip)/nrow(qdat)
# nearly 3695 missing zip codes missing from ACS census data - not sure why this is
length(unique(qdat$zip[!qdat$zip %in% dat$zip]))
# remove those that are missing from the source zip file
qdat = qdat[qdat$zip %in% dat$zip, ]

# create a sampling probability for the zip code based on how many queries per day
# number of queries per day/ total # of queries that day
probs = qdat %>%
  select(zip, QueryDate) %>%
  group_by(QueryDate, zip) %>%
  summarise(sample_prob = n()) %>% # create the numerator n of queries on day t
  group_by(QueryDate) %>%
  mutate(sample_prob = sample_prob/n()) %>% # create the denominator - total number of queries on that day
  ungroup() %>%
  filter(QueryDate >= "2014-12-20" & QueryDate <= "2014-12-30") %>%
  group_by(zip) %>%
  summarise(sample_prob = median(sample_prob)) %>%
  top_n(100, sample_prob) 

# cases for demo
demo_zips = probs$zip

set.seed(1223)
newdat = qdat %>%
  filter(QueryDate >= "2014-12-22" & QueryDate <= "2014-12-28") %>%
  filter(zip %in% demo_zips) %>%
  group_by(QueryDate) %>%
  mutate(A1_demo = sample(finalCode, size=1)) %>%
  ungroup()

# save the file

# Create Dates object
dates <- sort(unique(newdat$date))

# CREATE THE CENSUS WEIGHTS
createMRPcensusweights <- function(natsamp){
  
  # Replacing population for zip code type
  natcensus <- aggregate(cbind(median_inc) ~ state+child.perhouse+educ_factor+age+educ.age, mean, data=natsamp)
  natcensus$median_inc = natcensus$V1
  natcensus$ziptype <- paste(natcensus$state, natcensus$child.perhouse,   
                             natcensus$educ.age, sep = "-")
  
  # get the number of people represented by each zip code type
  natsum <- aggregate(pop ~ state+child.perhouse+educ_factor+age+educ.age, sum, data=natsamp)
  natsum$ziptype = paste(natcensus$state, natcensus$child.perhouse,  
                         natcensus$educ.age, sep = "-")
  natsum = natsum[, c("ziptype", "pop")]
  
  natcensus = merge(natcensus, natsum, by="ziptype")
  #combine state sums to get state level percents
  zippops = aggregate(pop~state, sum, data=natsamp)
  zippops = reshape::rename(zippops, c(pop="total_state_pop"))
  natcensus = merge(zippops, natcensus, by="state")
  natcensus$pctstate <- natcensus$pop/natcensus$total_state_pop #produce zip-level person-type proportions, divide by state pop
  
  return(natcensus)
}
mrpweights <- createMRPcensusweights(natsamp = dat) #create census weights based on population sizes

# MERGE CENSUS FEATURES WITH THE QUERY DATA
qdat <- merge(newdat, dat, by="zip")

write.csv(qdat, "~/Documents/ms_flu/data/mrp_example_data.csv")
