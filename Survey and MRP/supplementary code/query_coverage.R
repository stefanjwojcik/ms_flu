#Plotting stuff going in different zip codes::

library(zipcode)
library(ggplot2)
library(data.table)
library(lubridate)
library(gganimate)
library(ggthemes)
library(stringr)
library(noncensus)
library(reshape2)

setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/")

# Loading the zip code data
dat <- readRDS("zipcodeCensusData_v2.rds")
dat$zip <- as.character(dat$zip)
dat$educ.age = factor(dat$educ.age)
dat$median_inc = scale(dat$median_inc)

# GET THE QUERIES DATA:
qdat <- read.csv("AllStates_Zipcode_fludata_nobadlines.csv", stringsAsFactors = F)
names(qdat) = c("V1", "QueryDate", "WeekID", "SessionID", "NormalizedQuery", "finalCode", "PostalCode", "Gender", "AgeInYears", "State")

# RECODING QUERIES DATA::
qdat$date <- as.Date(qdat$QueryDate)
qdat$zip <- as.character(qdat$PostalCode)
qdat$A1 <- 1*(qdat$finalCode=="a1") #|qdat$finalCode=="b1" if we want to include secondary symptoms
qdat$month = factor(month(qdat$date), ordered = T)
qdat$year = year(qdat$date)
qdat = qdat[tolower(qdat$State) %in% tolower(c(state.name, "District Of Columbia")),  ]
qdat = qdat[nchar(qdat$zip)==5, ]
qdat = qdat[qdat$zip %in% dat$zip, ]

# MERGE CENSUS FEATURES WITH THE QUERY DATA
qdat <- merge(qdat, dat, by="zip")

# GENERATE STATE-LEVEL AGGREGATES - THE FLU QUERIES DATA
norm_queries = qdat %>%
  select(State, pop, year.x, A1) %>%
  group_by(State, year.x) %>%
  summarise(qcount = n()/mean(pop), A1count = sum(A1)/mean(pop)) %>%
  group_by(State) %>%
  summarise(qcount = mean(qcount), A1count = mean(A1count)) %>%
  mutate(Flu_count = (qcount-mean(qcount))/sd(qcount), A1_count = (A1count - mean(A1count))/sd(A1count)) %>%
  select(State, Flu_count, A1_count)
# Factor according to size of state counts
norm_queries$State = factor(norm_queries$State, levels = unique(norm_queries$State[order(norm_queries$Flu_count)]))

# Reshape to Long
flu_qstates <- melt(norm_queries, id.vars="State")

### GENERATE YEAR-LEVEL AGGREGATES - FLU QUERIES
flu_qyears = qdat %>%
  select(State, pop, year.x, A1) %>%
  group_by(year.x) %>%
  summarise(qcount = n(), A1count = sum(A1)) %>%
  mutate(Flu_count = (qcount-mean(qcount))/sd(qcount), A1_count = (A1count - mean(A1count))/sd(A1count)) %>%
  mutate(Year = year.x) %>%
  select(Year, Flu_count, A1_count)
# Factor according to size of year counts
flu_qyears = melt(flu_qyears, id.vars = "Year")

##### ALL QUERIES ############

# Importing data from the 'noncensus' package
data(states) 
states = states[, c("name","population")]
states$name = gsub(" ", ".", states$name)
# IMPORTING - ALL QUERY DENOMINATORS
all_qs = read.csv("SRPV_AllStates.csv", stringsAsFactors = F)
all_qs$Year = str_sub(all_qs$WeekID, -4) # take last four digits to attain the year
all_qs$WeekID = NULL # remove the source week ID variable
all_qs = melt(all_qs, id.vars = c("Year"))
all_qs$variable = gsub("\\.", " ", all_qs$variable)

# GENERATE STATE-LEVEL AGGREGATES AMONG ALL QUERIES
all_qstates = all_qs %>%
  group_by(variable) %>%
  summarise(q_all_count = mean(value)) %>%
  mutate(all_queries = (q_all_count-mean(q_all_count))/sd(q_all_count)) %>%
  mutate(State = variable) %>%
  select(State, all_queries)

# GENERATE YEAR-LEVEL AGGREGATES AMONG ALL QUERIES
all_qyears = all_qs %>%
  group_by(Year) %>%
  summarise(all_queries = mean(value)) %>%
  mutate(all_queries = (all_queries-mean(all_queries))/sd(all_queries))

# BIND STATE-LEVEL
state_level = data.frame(State = c(as.character(flu_qstates$State), as.character(all_qstates$State)), 
                         variable = c(as.character(flu_qstates$variable), rep("Query_count", nrow(all_qstates))), 
                         value = c(flu_qstates$value, all_qstates$all_queries))
# BIND YEAR-LEVEL
year_level = data.frame(Year = c(as.character(flu_qyears$Year), as.character(all_qyears$Year)), 
                         variable = c(as.character(flu_qyears$variable), rep("Query_count", nrow(all_qyears))), 
                         value = c(flu_qyears$value, all_qyears$all_queries))

# PLOT STATE DATA the data as a dot plot
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

o = order((state_level %>% group_by(State) %>% summarise(mean(value)))$`mean(value)`)
state_level$State = factor(state_level$State, 
                           levels = state_level$State[o])
p = ggplot(state_level, aes(y = State, x = value, col = variable, fill=variable)) +  
  geom_point(alpha=.5, size=2) + xlab("Avg Annual Query Counts Normalized by State Population, Converted to Z-score (2012-2017)") 
p + scale_colour_manual(values=cbbPalette)

# PLOT YEAR DATA the data as a dot plot
p = ggplot(year_level, aes(y = Year, x = value, col = variable, fill=variable)) +  
  geom_point(alpha=.5, size=5) + xlab("Avg Annual Query Counts, Converted to Z-score (2012-2017)") 
p + scale_colour_manual(values=cbbPalette)

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

###### ALL QUERIES

setwd("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/")
  


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
