# Example MRP smoothing algo file

setwd("~/Documents/ms_flu/")
#setwd("/home/stefanwojcik/flu")

dat <- readRDS("zipcodeCensusData_v2.rds")
dat$zip <- as.character(dat$zip)
dat$educ.age = factor(dat$educ.age)
dat$median_inc = scale(dat$median_inc)


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

qdat = read.csv("data/mrp_example_data.csv")


dates <- sort(unique(qdat$date))

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
  print(dates[i])
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
