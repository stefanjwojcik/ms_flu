#processing flu 
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

# Create Dates object
dates <- sort(unique(qdat$date))

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
  zippops = rename(zippops, c(pop="total_state_pop"))
  natcensus = merge(zippops, natcensus, by="state")
  natcensus$pctstate <- natcensus$pop/natcensus$total_state_pop #produce zip-level person-type proportions, divide by state pop
  
  return(natcensus)
}
mrpweights <- createMRPcensusweights(dat) #create census weights based on population sizes

# MERGE CENSUS FEATURES WITH THE QUERY DATA
qdat <- merge(qdat, dat, by="zip")
# SCALE MEDIAN INCOME FOR THE MULTILEVEL MODEL
#qdat$median_inc = scale(qdat$median_inc) # 

### CREATING THE MRP ALGORITHM::::::
#Define a bunch of placeholder objects
n.states <- length(unique(mrpweights$state)) #based on the census set
#Actual candidate prediction matrix
A1pred <- numeric(length(unique(dates))*n.states)  # n.rows=n.dates
#names of states in prediction holder
A1pred.state <- rep(names(tapply(0*mrpweights$pctstate, mrpweights$state, sum)), length(dates)) 
#dates in prediction, made in advance
A1pred.dates <- rep(dates, each=n.states)

### FOCUSING ON ZIP CODES
n.zips = length(unique(mrpweights$zip))
A1predzip <- numeric(length(unique(dates))*n.zips)  # n.rows=n.dates
A1pred.zip <- rep(names(tapply(0*mrpweights$pctstate, mrpweights$zip, sum)), length(dates)) 
#dates in prediction, made in advance
A1pred.dates <- rep(dates, each=n.zips)

#
#listobj <- list()
#iterate over the 7-day window and the candidate list to predict the likelihood of a person making a tweet for that candidate
data <- qdat
window.length = 3

A1signal <- foreach(i = window.length:length(dates)) %dopar% { # NEED TO DEFINE A WINDOW LENGTH::::
  #date index
  dates.tmp <- data$date %in% dates[i:(i-window.length)]
  #temporary dataset
  tmp.dat <- data.frame(data[dates.tmp, ])
  
  #Remove no-variance factors
  fac.list <- list( state = tmp.dat$State, educ_factor = tmp.dat$educ_factor, age = tmp.dat$age, 
                    educ.age = tmp.dat$educ.age, child.perhouse=tmp.dat$child.perhouse) # state+race.white+child.perhouse+age+educ.income
  fac.dims <- sapply(lapply(fac.list, function(x) table(x)[table(x)>0]), function(x) length(x)>1) #does the factor have any variance? if no, don't build a model with it..
  dyn.form <- paste("(1|", names(fac.dims)[fac.dims], ")", collapse="+", sep="")
  if(sum(fac.dims)==0){
    dyn.form <- paste("1")
  }
  fixef.dims <- list(median_inc=tmp.dat$median_inc) # individual-level fixed effect
  dyn.form <- paste(paste(names(fixef.dims), sep="", collapse="", "+"), dyn.form)
  #Run the model and fill in the boxes if there is variance and a model can be run
  
  if(sum(tmp.dat$A1, na.rm=T)>0 && nrow(tmp.dat)>0){
    mod <- try(glmer(as.formula(paste("A1~", dyn.form)), data=tmp.dat, 
              control = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 5e8)),
              family=binomial(link="logit")), silent=T)
    if(class(mod) != "try-error" && nrow(data[data$date %in% dates[i], ]) != 0 ){
      cell.pred <- na.omit(predict(mod, allow.new.levels=T, newdata=mrpweights, type="response")) #this generates a cell-level prediction
      cell.pred.weighted <- cell.pred*mrpweights$pctstate  #now we need to weight the cell-level prediction by the frequency
      avg.census.bystate <- tapply(cell.pred.weighted, mrpweights$state, sum) # temporary vector of length n.state
      cell.pred.weighted.bystate <-  100* as.vector(avg.census.bystate) # temporary vector of length n.state
      cell.pred.weighted.bystate #predicts into the empty vector
      
    } else { # if there is an error with the model, such as no variance on a given day
      cell.pred = 0*mrpweights$pctstate
      as.vector(tapply(cell.pred, mrpweights$state, sum))
    }
  } else { # or if there are no data for a given day
    cell.pred.weighted <- 0*mrpweights$pctstate
    as.vector(tapply(cell.pred.weighted, mrpweights$state, sum))
  }
  #print(paste(dates[i], "completed", sep=", "))
} #END of i statements loop

# compile into dataframe
a1df = do.call(rbind, A1signal)
colnames(a1df) = as.character(unique(mrpweights$state))
a1df = as.data.frame(a1df)
a1df$date = dates[window.length:length(dates)]
write.csv(a1df, "/home/stefanwojcik/flu/estimates_flu_states_3daywindowV7.csv")


getLoess = function(series){
  time = 1:length(series)
  lo = predict(loess.as(time, series, degree = 2, criterion = "gcv", user.span = NULL, plot = F))
  return(lo)
}

##############
a1df_smooth = apply(a1df[, !grepl("date", names(a1df))], 2, function(x) getLoess(x))
a1df_smooth = as.data.frame(a1df_smooth)
a1df_smooth$date = a1df$date
write.csv(a1df_smooth, "MRP_states_3daywindow_3Nov2017_post_loess.csv")
write.csv(a1df, "MRP_states_3daywindow_3Nov2017.csv")
##########################

a1stack = data.frame(state=stack(flu[, c("CA", "MI")]), date=rep(flu$dates, times=4))
ggplot(a1stack, aes(x=date, y=state.values, colour=state.ind))+ geom_smooth( span= .1, n=350) + geom_line(stat="identity", alpha=.2) + ylim(0,100)
qplot(date, state.values, colour=state.ind, dat=a1stack)

###########
### MODEL TESTING ############
samp = sample(1:nrow(qdat), 2000)
truth = aggregate(A1 ~ State, mean, data = qdat)
agg = aggregate(A1 ~ State, mean, data = qdat[samp, ])

# here is the model
qdat$median_inc = scale(qdat$median_inc)
qdat$educ = scale(qdat$educ)
mod = glmer(A1 ~ educ + pov.pct + health.pct + (1|state)+(1|race.white)+(1|child.perhouse)+(1|age), 
            data = qdat[samp,], 
            family = binomial(link = "logit"))
library(caret)
qdat$pred = NA
qdat$pred[samp] = predict(mod, type = "response")
agg$mrp = aggregate(pred ~ State, mean, data = qdat[samp, ])[,2]

postResample(truth$A1, agg$A1)
postResample(truth$A1, agg$mrp)

######
state = read.csv("~/Downloads/State Flu Data - All States.csv")
state = state[which(state$State=="CA"), c("Week.Ending", "State", "Positive.Lab.Tests")]
state$dates = as.Date(state$Week.Ending, format= "%m/%d/%Y")
ca = flu[, c("CA", "dates")]
ca$dates = as.Date(ca$dates)
ca = merge(state, ca, by="dates", all.y=T)
#########

# here is the original data
agstate = aggregate(A1~state+date, mean, data=qdat)
ggplot(agstate, aes(x=date, y=A1, colour=state)) + geom_smooth( span= .1, n=350)  + geom_point(stat="identity", alpha=.2) + ylim(0,1)


# HERE IS JUST THE AGGREGATED DATA
aggdat <- aggregate(A1~date, sum, data=data)
qplot(date, A1, data=aggdat) + geom_smooth() + xlab("Dates") + ylab("Raw A1 Search Volume") + ylim(0, 100)
# HERE IS THE SMOOTHED SIGNAL
dates2 <- dates[window.length:length(dates)]
A1df <- data.frame(dates = dates2, A1 = unlist(A1signal))
qplot(dates, A1, data=A1df) + geom_smooth(span=50) + xlab("Dates") + ylab("Estimated Proportion of Population")


qplot(dates, A1, data=A1df) + geom_line() + xlab("Dates") + ylab("Estimated Proportion of Population")

## CORRELATION BETWEEN THE TWO
qplot(A1df$A1, aggdat$A1[window.length:length(dates)], col=dates[window.length:length(dates)])  + xlab("Smoothed A1") + ylab("Raw Volumes") + ylim(0, 1100)

# Wrote file out:
# write.csv(A1df, "/Volumes/TINY CRYPT/papers/Working Projects/Lazer Lab/Flu/Clean Data/A1smooth_2012-2016_byweek.csv")

