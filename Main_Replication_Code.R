
library(ggplot2)
library(stargazer)
library(effects)
library(Zelig)
library(noncensus)
library(texreg)
library(dplyr)
library(tidytext)
library(SnowballC)
library(data.table)
library(irr)

setwd("~/Google Drive/")
d1 = readRDS("~/Documents/ms_flu/data/main_flu_dat.rds")

# Percent in respondents of age categories:
Age = factor(d1$age, labels = c("Below 18", 
                                "18-24", 
                                "25-34", 
                                "35-44", 
                                "45-54",
                                "55-64",
                                "65+"))
round(table(Age)/nrow(d1)*100, digits=0)

# Percent in respondents of gender categories:
round(table(d1$female)/nrow(d1)*100, digits=0)

# Flu search and gender among survey respondents: row by row
  # Any flu term for men
table(d1$any.flu[d1$female==0] > 0)/nrow(d1[d1$female==0, ])
table(d1$any.flu[d1$female==1] > 0)/nrow(d1[d1$female==1, ])
table(d1$any.flu > 0)/nrow(d1)

# OVERALL SUMMARY TABLE
mod_names = names(d1)
nice_names = c("A1", "A2", "B1", "B2", "Any.Flu.Term", "Search.Volume", 
               "Female", "Parent", "Spouse", "Age", "Household.Flu", 
               "Respondent.Flu", "Spouse.Flu", "Child.Flu", "Primary.User", 
               "Education", "Race", "Early.Response")
names(d1) = nice_names
stargazer(d1, type="latex", header=F)


##
table(d1$A1)/nrow(d1)
table(d1$A2)/nrow(d1)
table(d1$B1)/nrow(d1)
dim(d1)
rm(d1)
##

# - Load panel queries here
queries = readRDS("~/Documents/ms_flu/data/queries.rds")

# Tidying and calculating the word frequencies in the queries for the whole panel
panelqueries = data.table(text = queries$query, n_query = 1:nrow(queries))
panelqueries = panelqueries %>% filter(!is.na(text))
clean_panel_queries <- panelqueries %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords('english'), by=c("word"="word")) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort=T) %>%
  mutate(proportion = n/sum(n))
# Subset to the top ten occurring words
clean_panel_queries = head(clean_panel_queries, 10)

##
# Get frequency of queries and 1-grams A1
A1queries = data.table(text = queries$query[queries$Final.Code=="A1"], n_query = 1:nrow(queries[queries$Final.Code=="A1", ]))
A1queries = A1queries %>% filter(!is.na(text))
clean_A1_queries <- A1queries %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords('english'), by=c("word"="word")) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort=T) %>%
  mutate(proportion = round(n/sum(n), 3) )
# Subset to top A1 queries
clean_A1_queries = head(clean_A1_queries, 10)

##

# QUERIES CONFUSION MATRIX
queries_confusion_matrix = table(queries$Coder2, queries$Coder1)

##
#Intercoder Reliability
queries_kappa = kappa2(queries[,c("Coder2", "Coder1")]) #Cohen's Kappa 
queries_agree = agree(queries[,c("Coder2", "Coder1")]) #Agreement 
##

# Read pages 
pages = readRDS("~/Documents/ms_flu/data/pages.rds")

# PAGES CONFUSION MATRIX
pages_confusion_matrix = table(pages$Coder2, pages$Coder1)

#Intercoder Reliability
pages_kappa = kappa2(pages[,c("Coder2", "Coder1")]) #Cohen's Kappa Carolina-Isys
pages_agree = agree(pages[,c("Coder2", "Coder1")]) #Agreement Carolina-Isys

#
# RELIABILITY OF EXPANDED QUERIES::
exp_qs = readRDS("~/Documents/ms_flu/data/expanded_queries.rds")
#Intercoder Reliability
bing_kappa = kappa2(exp_qs[,c("Coder1", "Coder2")]) #Cohen's Kappa 
bing_agree = agree(exp_qs[,c("Coder1", "Coder2")]) #Agreement 

# Get frequency of queries and 1-grams OVERALL - slight change in number of true words due to 
# minor change in stopwords in 
bing_queries = data.table(text = as.character(exp_qs$query), n_query = 1:nrow(exp_qs))
bing_queries = bing_queries %>% filter(!is.na(text))
clean_bing_queries <- bing_queries %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords('english'), by=c("word"="word")) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort=T) %>%
  mutate(proportion = round(n/sum(n), 3) )
# Subset to top A1 queries
clean_bing_queries = head(clean_bing_queries, 10)
# create confusion matrix
bing_confusion_matrix = table(exp_qs$Coder1, exp_qs$Coder2)

# bing queries stemmed - A1
exp_qs_final = readRDS("~/Documents/ms_flu/data/expanded_queries_final.rds")
bing_A1_queries = data.table(text = as.character(exp_qs_final$query[exp_qs_final$code=="a1"]), n_query = 1:nrow(exp_qs_final[exp_qs_final$code=="a1", ]))
bing_A1_queries = bing_A1_queries %>% filter(!is.na(text))
clean_bing_A1_queries <- bing_A1_queries %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords('english'), by=c("word"="word")) %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort=T) %>%
  mutate(proportion = round(n/sum(n), 3) )
# Subset to top A1 queries
clean_bing_A1_queries = head(clean_bing_A1_queries, 10)

####### PRINT FIRST SET OF TEXT OBJECTS AS THEY APPEAR IN THE SI ###########

# intercoder reliability table
queries_kappa
queries_agree
pages_kappa
pages_agree

# confusion matrices
queries_confusion_matrix
pages_confusion_matrix

# word frequencies tables
stargazer(clean_panel_queries, summary=F, title="Top Stemmed Words in All Queries")
stargazer(clean_A1_queries, summary=F, title="Top Stemmed Words in A1 Queries")

# bing intercoder reliability
bing_kappa
bing_agree

# bing confusion matrix
bing_confusion_matrix

# bing expanded queries
stargazer(clean_bing_queries, summary=F, title = "Top Stemmed Words in All Queries (Bing)")
stargazer(clean_bing_A1_queries, summary=F, title = "Top Stemmed words in A1 Queries (Bing)")

####################################

##
## Descriptive Statistics of the Survey Data
d1 = readRDS("~/Documents/ms_flu/data/main_flu_dat.rds")
mod_names = names(d1)
nice_names = c("A1", "A2", "B1", "B2", "Any.Flu.Term", "Search.Volume", 
               "Female", "Parent", "Spouse", "Age", "Household.Flu", 
               "Respondent.Flu", "Spouse.Flu", "Child.Flu", "Primary.User", 
               "Education", "Race", "Early.Response")
names(d1) = nice_names
table(d1$A1, d1$Household.Flu)
d1 %>% group_by(Household.Flu) %>% summarise(mean(A1))
d1 %>% group_by(A1) %>% summarise(mean(Household.Flu))

# create A2 variable where the household has not also executed an A1 search
d1$A2[which(d1$A1==1 & d1$A2==1)] = NA
table(d1$A2, d1$Household.Flu)
d1 %>% group_by(Household.Flu) %>% summarise(mean(A2, na.rm=T))
d1 %>% group_by(A2) %>% summarise(mean(Household.Flu))
##

## Get tau value based on prior rate of search for an observed season::
tau = readRDS("~/Documents/ms_flu/data/tau.rds")
tau = tau$numer/tau$denom
##

##
fixTau = function(yhat, tau){
  log( ( (1 - tau) / tau) * (yhat / (1 - yhat) ) )
}
#Models of flu search - A1 and A2
names(d1) = mod_names
ma1 <- zelig(a1~household.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1) 
ma2 <- zelig(a1~ch.flu+volume+female+age, model="relogit", tau=tau, data=d1)
ma3 <- zelig(a1~r.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1)
ma4 <- zelig(a1~s.flu+volume+female+age, model="relogit", tau=tau, data=d1) #
texreg(c(ma1, ma2, ma3, ma4))
##

# Alternative A2 model:
ma12 <- zelig(a2~household.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1) 
ma22 <- zelig(a2~ch.flu+volume+female+age, model="relogit", tau=tau, data=d1)
ma32 <- zelig(a2~r.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1)
ma42 <- zelig(a2~s.flu+volume+female+age, model="relogit", tau=tau, data=d1) #
texreg(c(ma12, ma22, ma32, ma42))

# Alternative models with race and education
ma5 <- zelig(a1~household.flu+education+race, model="relogit", tau=tau, data=d1) 

# Alternative models with early response
ma6a <- zelig(household.flu~volume+female+parent+age+early_response, model = "logit", data=d1)
ma6b <- zelig(r.flu~volume+female+parent+age+early_response, model = "logit", data=d1)
ma6c <- zelig(s.flu~volume+female+parent+age+early_response, model = "logit", data=d1)
ma6a1 <- zelig(a1 ~ household.flu +volume+female+parent+age+early_response, model = "logit", data=d1)

#### Estimating THE EFFECT OF VOLUME
set.seed(765)
#volume <- c(5.88, 9.49)  #from median to max
summary(d1$volume)
volume <- c(4.852, 6.661)  #from third quartile to first
ma1X <-setx(ma1, volume = volume)  # Simulate quantities of interest 
ma1sim <- sim(ma1, x = ma1X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma1sim)
#lo_vol = mean(df$expected_value[df$volume==5.88])
#hi_vol = mean(df$expected_value[df$volume==9.49])
lo_vol = mean(df$expected_value[df$volume==4.852])
hi_vol = mean(df$expected_value[df$volume==6.661])
##
# RR:
hi_vol/lo_vol
# RD: 
hi_vol-lo_vol
##
##


##
#### PLOTTING THE EFFECT OF THE FLU - Household Flu
set.seed(765)
household.flu.r <- c(0, 1) 
ma1X <-setx(ma1, household.flu = household.flu.r)  # Simulate quantities of interest 
ma1sim <- sim(ma1, x = ma1X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma1sim)
mflu = mean(df$expected_value[df$household.flu==1])
mnoflu = mean(df$expected_value[df$household.flu==0])
df$HouseholdFlu = as.factor(df$household.flu)
g = ggplot(df, aes(x=expected_value, fill=HouseholdFlu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate") + ylab("Density")
g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)
##

##
# RR:
mflu/mnoflu
# RD: 
mflu-mnoflu
##

# model with race and education
texreg(c(ma5))

##
#Primary users? - Looking at Dads and Moms who are the only users of the computer
d1$Parent.Gender <- factor(d1$female, labels=c("Dads", "Moms"))
ma2 <- zelig(a1~ch.flu*Parent.Gender+volume+age, model="relogit", tau=tau, data=subset(d1, prm.user==T))
ma4 <- zelig(a2~ch.flu*Parent.Gender+volume+age, model="relogit", tau=tau, data=subset(d1, prm.user==T)) 
##

##
texreg(c(ma2, ma4))
# PLOTTING MA2 - DADS
require(scales)
set.seed(765)
ch.flu <- c(0,1)
Parent.Gender = c("Dads")
ma2X <-setx(ma2, ch.flu = ch.flu, Parent.Gender=Parent.Gender)  # Simulate quantities of interest 
ma2sim <- sim(ma2, x = ma2X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma2sim)
mflu = mean(df$expected_value[df$ch.flu==1])
mnoflu = mean(df$expected_value[df$ch.flu==0])
df$childflu = as.factor(df$ch.flu)
g = ggplot(df, aes(x=expected_value, fill=childflu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate - Fathers") + ylab("Density")
g = g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)
g + scale_x_continuous(labels = scientific) # change to labels = comma for non-scientific notation
# RR:
mflu/mnoflu
# RD: 
mflu-mnoflu
##

##
# PLOTTING MA2 - MOMS
set.seed(765)
ch.flu <- c(0,1)
Parent.Gender = c("Moms")
ma2X <-setx(ma2, ch.flu = ch.flu, Parent.Gender=Parent.Gender)  # Simulate quantities of interest 
ma2sim <- sim(ma2, x = ma2X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma2sim)
mflu = mean(df$expected_value[df$ch.flu==1])
mnoflu = mean(df$expected_value[df$ch.flu==0])
df$childflu = as.factor(df$ch.flu)
g = ggplot(df, aes(x=expected_value, fill=childflu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate - Mothers") + ylab("Density")
g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)
##

## Classification
# REOPEN QUERIES AND PAGES
#Queries:
queries <- readRDS("~/Documents/ms_flu/data/queries.rds")
# Pages:
pages <- readRDS("~/Documents/ms_flu/data/pages.rds")
##

##
#open panel demographics:
dat = read.csv("~/Documents/ms_flu/data/panel_demographics.csv")
# the query data
full = rbind(na.omit(pages[, c("QID2", "Final.Code")]), na.omit(queries[, c("QID2", "Final.Code")]))
##

##
# Initial merge to match respondents to queries
full = merge(full, dat[, c("RID", "QID2")], by="QID2")
# Aggregate to get the total number of each type of search per individual
c = model.matrix(~0+Final.Code, data=full) # binarizes the flu query code 
full = cbind(c, full) # binds data to full data
agg = aggregate(cbind(Final.CodeA1, Final.CodeA2, Final.CodeB1, Final.CodeB2, Final.CodeC1, Final.CodeD)~RID, sum, data=full) # aggregate number of searches per RID
##

##
# Final merge to get respondents to query volumes
full = merge(agg, dat, by="RID")
full$X = NULL
##

##
## ML Modeling 
library(caret)
set.seed(123654)
predictors = names(full)[!grepl("RID|pyoungchild|QID2|rflu|chflu", names(full))]
full$hflu = factor(full$hflu, labels=c("no_flu", "flu"))
full$rflu = factor(full$rflu, labels=c("no_flu", "flu"))
full$chflu = factor(full$chflu, labels=c("no_flu", "flu"))
##


##
################ of Household Flu ######################################
hfull = na.omit(full[, predictors])
tr = createDataPartition(hfull$hflu, times=1, p=.7)$Resample1
te = which(! 1:nrow(hfull) %in% tr )
######## BASE RATE for training and test data
postResample( rep("no_flu", nrow(hfull[tr,])) , hfull$hflu[tr])
postResample( rep("no_flu", nrow(hfull[te,])) , hfull$hflu[te])
table(hfull$hflu[tr])/nrow(hfull[tr,])
table(hfull$hflu[te])/nrow(hfull[te,])
##

##
trC = trainControl(method="boot")
mod_house = train(hflu~., data=hfull[tr, predictors], method="rf", trControl=trC, tuneGrid=expand.grid(mtry=2))
#postResample(predict(mod_house, newdata=hfull[te, predictors]), hfull$hflu[te])
##

##
# Confusion matrix of testing data
confusionMatrix(predict(mod_house, newdata=hfull[te, predictors]), hfull$hflu[te], positive="flu")
plot(varImp(mod_house))
##

## Alternative models that incorporate sore throat in panel analysis and classification ####

## ALTERNATIVE
d1 = readRDS("~/Documents/ms_flu/data/main_flu_dat_sorethroat.rds")
table(d1$a1)/nrow(d1)
table(d1$a2)/nrow(d1)
table(d1$b1)/nrow(d1)
dim(d1)
rm(d1)
##

## ALTERNATIVE
## Descriptive Statistics of the Survey Data
d1 = readRDS("~/Documents/ms_flu/data/main_flu_dat_sorethroat.rds")
mod_names = names(d1)
nice_names = c("A1", "A2", "B1", "B2", "Any.Flu.Term", "Search.Volume", 
                "Female", "Parent", "Spouse", "Age", "Household.Flu", 
               "Respondent.Flu", "Spouse.Flu", "Child.Flu", "Primary.User", 
               "Education", "Race")

names(d1) = nice_names
table(d1$A1, d1$Household.Flu)
round(table(d1$A1, d1$Household.Flu)/nrow(d1), digits=2)

table(d1$A2, d1$Household.Flu)
round(table(d1$A2, d1$Household.Flu)/nrow(d1), digits=2)
stargazer(d1, type="latex", header=F)
##

##ALTERNATIVE
tau = readRDS("~/Documents/ms_flu/data/tau.rds")
tau = tau$numer/tau$denom
##

##ALTERNATIVE
## Re-running the main analysis with the new measure
fixTau = function(yhat, tau){
  log( ( (1 - tau) / tau) * (yhat / (1 - yhat) ) )
}
#ALTERNATIVE Models of flu search - A1 and A2
names(d1) = mod_names
ma1 <- zelig(a1~household.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1) 
ma2 <- zelig(a1~ch.flu+volume+female+age, model="relogit", tau=tau, data=d1)
ma3 <- zelig(a1~r.flu+volume+female+parent+age, model="relogit", tau=tau, data=d1)
ma4 <- zelig(a1~s.flu+volume+female+age, model="relogit", tau=tau, data=d1) #
texreg(c(ma1, ma2, ma3, ma4))
##

# Alternative models with race and education
ma5 <- zelig(a1~household.flu+education+race, model="relogit", tau=tau, data=d1) 

##
#### ALTERNATIVE PLOTTING THE EFFECT OF THE FLU - Household Flu
set.seed(765)
household.flu.r <- c(0, 1) 
ma1X <-setx(ma1, household.flu = household.flu.r)  # Simulate quantities of interest 
ma1sim <- sim(ma1, x = ma1X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma1sim)
mflu = mean(df$expected_value[df$household.flu==1])
mnoflu = mean(df$expected_value[df$household.flu==0])
df$HouseholdFlu = as.factor(df$household.flu)
g = ggplot(df, aes(x=expected_value, fill=HouseholdFlu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate") + ylab("Density")
g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)

# ALTERNATIVE RR:
mflu/mnoflu
# RD: 
mflu-mnoflu
##

## ALTERNATIVE
texreg(c(ma1, ma2, ma3, ma4))
##
texreg(c(ma5))

## ALTERNATIVE
#Primary users? - Looking at Dads and Moms who are the only users of the computer
d1$Parent.Gender <- factor(d1$female, labels=c("Dads", "Moms"))
ma2 <- zelig(a1~ch.flu*Parent.Gender+volume+age, model="relogit", tau=tau, data=subset(d1, prm.user==T))
ma4 <- zelig(a2~ch.flu*Parent.Gender+volume+age, model="relogit", tau=tau, data=subset(d1, prm.user==T)) 
##

## ALTERNATIVE
texreg(c(ma2, ma4))
# PLOTTING ALTERNATIVE MA2 - DADS
set.seed(765)
ch.flu <- c(0,1)
Parent.Gender = c("Dads")
ma2X <-setx(ma2, ch.flu = ch.flu, Parent.Gender=Parent.Gender)  # Simulate quantities of interest 
ma2sim <- sim(ma2, x = ma2X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma2sim)
mflu = mean(df$expected_value[df$ch.flu==1])
mnoflu = mean(df$expected_value[df$ch.flu==0])
df$childflu = as.factor(df$ch.flu)
g = ggplot(df, aes(x=expected_value, fill=childflu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate - Fathers") + ylab("Density")
g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)
# RR:
mflu/mnoflu
# RD: 
mflu-mnoflu
##

## ALTERNATIVE
# PLOTTING MA2 - MOMS
set.seed(765)
ch.flu <- c(0,1)
Parent.Gender = c("Moms")
ma2X <-setx(ma2, ch.flu = ch.flu, Parent.Gender=Parent.Gender)  # Simulate quantities of interest 
ma2sim <- sim(ma2, x = ma2X)  # Extract expected values from simulations 
df = zelig_qi_to_df(ma2sim)
mflu = mean(df$expected_value[df$ch.flu==1])
mnoflu = mean(df$expected_value[df$ch.flu==0])
df$childflu = as.factor(df$ch.flu)
g = ggplot(df, aes(x=expected_value, fill=childflu)) 
g = g + geom_density(alpha=.5) + xlab("Expected A1 Search Rate - Mothers") + ylab("Density")
g + geom_vline(xintercept = mflu) + geom_vline(xintercept = mnoflu)
##

## ALTERNATIVE
#open panel demographics:
#Pages
pages <- readRDS("~/Documents/ms_flu/data/pages.rds")
##

## ALTERNATIVE
#Queries:
queries <- readRDS("~/Documents/ms_flu/data/queries.rds")
##

## We re-run the classification model in similar fashion as above

# load fresh data
dat = read.csv("~/Documents/ms_flu/data/panel_demographics_sorethroat.csv")
# the query data
full = rbind(na.omit(pages[, c("QID2", "Final.Code")]), na.omit(queries[, c("QID2", "Final.Code")]))
# Initial merge to match respondents to queries
full = merge(full, dat[, c("RID", "QID2")], by="QID2")
# Aggregate to get the total number of each type of search per individual
c = model.matrix(~0+Final.Code, data=full) # binarizes the flu query code 
full = cbind(c, full) # binds data to full data
agg = aggregate(cbind(Final.CodeA1, Final.CodeA2, Final.CodeB1, Final.CodeB2, Final.CodeC1, Final.CodeD)~RID, sum, data=full) # aggregate number of searches per RID
# Final merge to get respondents to query volumes
full = merge(agg, dat, by="RID")
full$X = NULL
## ML Modeling 
library(caret)
set.seed(123654)
predictors = names(full)[!grepl("RID|pyoungchild|QID2|rflu|chflu", names(full))]
full$hflu = factor(full$hflu, labels=c("no_flu", "flu"))
full$rflu = factor(full$rflu, labels=c("no_flu", "flu"))
full$chflu = factor(full$chflu, labels=c("no_flu", "flu"))
################ of Household Flu ######################################
hfull = na.omit(full[, predictors])
tr = createDataPartition(hfull$hflu, times=1, p=.7)$Resample1
te = which(! 1:nrow(hfull) %in% tr )
######## BASE RATE
postResample( rep("no_flu", nrow(hfull[tr,])) , hfull$hflu[tr])
postResample( rep("no_flu", nrow(hfull[te,])) , hfull$hflu[te])
table(hfull$hflu[tr])/nrow(hfull[tr,])
table(hfull$hflu[te])/nrow(hfull[te,])
# bootstrap validation
trC = trainControl(method="boot")
mod_house = train(hflu~., data=hfull[tr, predictors], method="rf", trControl=trC, tuneGrid=expand.grid(mtry=2))
#postResample(predict(mod_house, newdata=hfull[te, predictors]), hfull$hflu[te])
# Confusion matrix of testing data
confusionMatrix(predict(mod_house, newdata=hfull[te, predictors]), hfull$hflu[te], positive="flu")
plot(varImp(mod_house))
##


