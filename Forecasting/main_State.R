rm(list=ls()) 
setwd("C:/Users/avbijral/Desktop/FluForecasting/")


source("StateFluPrediction_routine.R")

logMRP <- 1
method = "ML"


## Search for best model using AIC and rolling train and test

State <- 'NM'
T2 <- 32*3
start <- 1
del_ind <- 0 # State Data starts at Different Dates
season <- 32 # Estimated Seasonality
StateFile <- "Data/NMRaw.csv"
lag_r = 0
lag_m = 0
res_NM <- StateResults(start = start, del_ind = del_ind, State = State, StateFile = StateFile, logMRP = logMRP, method = method, season = season, T2=T2,lag_m = lag_m, lag_r = lag_r) 

State <- 'NY'
start <- 1
T2 <- 3*32
del_ind <- 2
season <- 37 # Estimated Seasonality
StateFile <- "Data/NYRaw.csv"
lag_r = 0
lag_m = 0
res_NY <- StateResults(start = start, del_ind = del_ind, State = State, StateFile = StateFile, logMRP = logMRP, method = method, season = season, T2=T2, lag_m=lag_m, lag_r = lag_r)


State <- 'DC'
T2 <- 3*32
start <- 1
del_ind <- 2
season <- 37# Estimated Seasonality
StateFile <- "Data/DCRaw.csv"
lag_m = 0
lag_r = 0
res_DC <- StateResults(start = start, del_ind = del_ind, State = State, StateFile = StateFile, logMRP = logMRP, method = method, season = season, T2=T2, lag_m=lag_m, lag_r = lag_r) 


State <- 'DE'
start <- 1
T2 <- 3*32
del_ind <- 2
season <- 30 # Estimated Seasonality
lag_r = 0
lag_m = 0
StateFile <- "Data/DERaw.csv"
res_DE <- StateResults(start = start, del_ind = del_ind, State = State, StateFile = StateFile, logMRP = logMRP, method = method, season = season, T2=T2, lag_m=lag_m, lag_r = lag_r) 
