rm(list=ls()) 
setwd("Forecasting/")


source("Supp.R")
require(zoo)


set.seed(5)
## US Flu Rates
start = 1
State = "US"
Z <- data.frame(read.csv("Data/USFlu.csv" , header = TRUE,stringsAsFactors = FALSE))
Z <- Z[start:nrow(Z),]
Yf <- na.approx((Z$Total))


## Rolling 3 Year Period

T1 = 1
T2 = 104+52

## Lasso (ARGO) with All Queries - Normalize and logit transformed 
X_q <- read.csv("Data/logit_national_queries.csv", header = T, stringsAsFactors = F)
# Compute CCF with the Flu series and keep only queries with > 0.5 correlation
cc <- apply(X_q,2, function(x) cor(Yf[1:T2],x[1:T2]))
ind <- which(cc > 0.5)
X_q <- X_q[,ind]


## Lasso with MRP adjusted A1 queries

Xmrp <- data.frame(read.csv("Data/mrp_national_queries.csv" , header = TRUE,stringsAsFactors = FALSE))


# Search Data Process and Merge with Flu Rates (match on existing weeks) (Logit Transformed)
Ymrp <- smoothA1Process("Data/estimates_flu_US_3daywindowV7.csv", State)
Yraw <- smoothA1Process("Data/USRaw.csv", State)
Yr <- read.csv("Data/logit_raw_all.csv",header = F, stringsAsFactors = F)
Yr <- Yr$V1

Ym <- na.approx(log(Ymrp[,3]))

# Train
# Regress at Lag
lag_m = 0  # US
lag_r = 0 # US
season = 52 # US
## Create the Data Frames for all the different Signals and Methods
# Adjust for Lags (Not Relevant Here)
if(lag_m==lag_r) {
  
  Ysub_mrp <- data.frame(cbind(Yf[(lag_m+1):length(Yf)], Ym[1:(length(Ym)-lag_m)])) # MRP
  Ysub_raw <- data.frame(cbind(Yf[(lag_r+1):length(Yf)], Yr[1:(length(Yr)-lag_r)])) # A1
  Ysub_all <- data.frame(cbind(Yf[(lag_r+1):length(Yf)], Yr[1:(length(Yr)-lag_r)], Ym[1:(length(Ym)-lag_m)])) # Raw
  Ysub_X <-  data.frame(cbind(Yf[(lag_r+1):length(Yf)], X_q)) # Raw
  
}
  
## Auto-Arima
## Auto-Arima

## Find the best model for SARIMA-HIST
require(forecast)
mdl_hist_auto <- auto.arima(ts(Ysub_mrp[T1:T2,1],frequency=season),stationary = T, approximation=F, parallel = T,stepwise = F, seasonal = T, ic = "aic",allowdrift = F, allowmean = F, max.D = 1)
print(mdl_hist_auto)
mdl_h_auto <- list()
mdl_params <- arimaorder(mdl_hist_auto)
print(mdl_params)
mdl_h_auto$p <- mdl_params[1]
mdl_h_auto$d <- mdl_params[2]
mdl_h_auto$q <- mdl_params[3]
if(length(mdl_params)>3) {
  mdl_h_auto$sp <- mdl_params[4]
  mdl_h_auto$sd <- mdl_params[5]
  mdl_h_auto$sq <- mdl_params[6]
} else {
  mdl_h_auto$sp <- 0
  mdl_h_auto$sd <- 0 #mdl_params[5]
  mdl_h_auto$sq <- 0
}


## Find the best model for SARIMA-MRP

mdl_mrp_auto <- auto.arima(ts(Ysub_mrp[T1:T2,1],frequency=season) ,stationary = T, approximation=F, xreg = Ysub_mrp[T1:T2,2],parallel = T,stepwise = F, seasonal = T, ic = "aic",allowdrift = F,allowmean = F, max.D=1)
print(mdl_mrp_auto)
mdl_m_auto <- list()
mdl_params <- arimaorder(mdl_mrp_auto)
print(mdl_params)
mdl_m_auto$p <- mdl_params[1]
mdl_m_auto$d <- mdl_params[2]
mdl_m_auto$q <- mdl_params[3]
if(length(mdl_params)>3) {
  mdl_m_auto$sp <- mdl_params[4]
  mdl_m_auto$sd <- mdl_params[5]
  mdl_m_auto$sq <- mdl_params[6]
}else {
  mdl_m_auto$sp <- 0
  mdl_m_auto$sd <- 0
  mdl_m_auto$sq <- 0
}


## Find the best model for SARIMA-A1

mdl_raw_auto <- auto.arima(ts(Ysub_raw[T1:T2,1],frequency=season) , stationary = T, approximation=F, xreg = Ysub_raw[T1:T2,2], parallel = T, stepwise = F, seasonal = T, ic = "aic",allowdrift = F, allowmean = F, max.D = 1)
print(mdl_raw_auto)
mdl_r_auto <- list()
mdl_params <- arimaorder(mdl_raw_auto)
print(mdl_params)
mdl_r_auto$p <- mdl_params[1]
mdl_r_auto$d <- mdl_params[2]
mdl_r_auto$q <- mdl_params[3]
if(length(mdl_params)>3) {
  mdl_r_auto$sp <- mdl_params[4]
  mdl_r_auto$sd <- mdl_params[5]
  mdl_r_auto$sq <- mdl_params[6]
}else {
  mdl_r_auto$sp <- 0
  mdl_r_auto$sd <- 0 #mdl_params[5]
  mdl_r_auto$sq <- 0
}



## Argo Model (All Queries)

require(xts)
require(argo)
timef <- as.Date(paste("0", Z$Week, sep="-"), format= "%w-%Y-%W")
timef[157] <- timef[156]
dates <- as.Date(timef,format="%Y-%m-%d")
Zflu <- xts(Ysub_X[,1], dates)
names(Zflu) <- "ILI"
Xog <- xts(Ysub_X[,2:ncol(Ysub_X)], dates)
Xmrp <- xts(sqrt(Xmrp[1:279,3:ncol(Xmrp)]), dates)


mdl_argo_hist <- argo(Zflu, exogen = NULL, N_lag = 1:52, N_training = T2-53, alpha = 1, use_all_previous = F)
mdl_argo_all <- argo(Zflu, exogen = Xog, N_lag = 1:52, N_training = T2-53, alpha = 1, use_all_previous = F)
mdl_argo_mrp <- argo(Zflu, exogen = Xmrp, N_lag = 1:52, N_training = T2-53, alpha = 1, use_all_previous = F)


# Compute Prediction Errors (Auto Arima)
method = "ML"
mdlx <- mdl_m_auto
mdlh <- mdl_h_auto
mdlr <- mdl_r_auto


rmse1step_m_auto <- nStepAheadAuto(Ysub = Ysub_mrp, mdlx, mdlh, ahead=1, method = method, season=season, T2=T2,incl_mean = F, incl_mean_hist = F)
rmse2step_m_auto <- nStepAheadAuto(Ysub = Ysub_mrp, mdlx, mdlh, ahead=2, method = method, season=season, T2=T2,incl_mean = F, incl_mean_hist = F)

rmse1step_r_auto <- nStepAheadAuto(Ysub = Ysub_raw, mdlr, mdl_h, ahead=1, method = method,season=season, T2=T2, incl_mean = F, incl_mean_hist = F)
rmse2step_r_auto <- nStepAheadAuto(Ysub = Ysub_raw, mdlr, mdl_h, ahead=2, method = method,season=season, T2=T2, incl_mean = F, incl_mean_hist = F)



## Compute RMSE


## OVERALL
y_actual <- Ysub_X[(T2+1):nrow(Ysub_X),1]

## Auto Arima Accuracy
# SARIMA-A1 (1-step and 2-step)
rmse_r_1step_auto <- sqrt(mean((y_actual-rmse1step_r_auto[,2])^2))
rmse_r_2step_auto <- sqrt(mean((y_actual[2:length(y_actual)]-rmse2step_r_auto[,2])^2))

#SARIMA-MRP (1-step and 2-step)
rmse_m_1step_auto <- sqrt(mean((y_actual-rmse1step_m_auto[,2])^2))
rmse_m_2step_auto <- sqrt(mean((y_actual[2:length(y_actual)]-rmse2step_m_auto[,2])^2))

#SARIMA-HIST (1-step and 2-step)
rmse_h_1step_auto <- sqrt(mean((y_actual[1:length(y_actual)]-rmse1step_m_auto[,3])^2))
rmse_h_2step_auto <- sqrt(mean((y_actual[2:length(y_actual)]-rmse2step_m_auto[,3])^2))

## ARGO 

pred_argo_hist <- na.omit(as.numeric(mdl_argo_hist$pred))
pred_argo_all <- na.omit(as.numeric(mdl_argo_all$pred))
pred_argo_mrp <- na.omit(as.numeric(mdl_argo_mrp$pred))

## Accuracy


# 1-step Accuracy
mrp_acc <- accuracy(rmse1step_m_auto[,2],y_actual) #SARIMA-MRP
hist_acc <- accuracy(rmse1step_m_auto[,3],y_actual) # SARIMA-HIST
raw_acc <- accuracy(rmse1step_r_auto[,2],y_actual) # SARIMA-A1
argo_hist_acc <- accuracy(pred_argo_hist,y_actual) # ARGO-HIST
argo_a1_acc <- accuracy(pred_argo_all,y_actual) # ARGO-A1

# 2-step
mrp_acc_2 <- accuracy(rmse2step_m_auto[,2],y_actual[2:length(y_actual)]) #SARIMA-MRP
raw_acc_2 <- accuracy(rmse2step_r_auto[,2],y_actual[2:length(y_actual)]) #SARIMA-A1
hist_acc_2 <- accuracy(rmse2step_r_auto[,3],y_actual[2:length(y_actual)]) # SARIMA-HIST

#save("US_2_14_2018.RData")

#resultsdf <- cbind.data.frame(Actual=y_actual, Hist=rmse1step_r_auto[,3], MRP=rmse1step_m_auto[,2], RAW = rmse1step_r_auto[,2], ARGO = pred_argo_all)
#resultsdf  <- cbind.data.frame(Actual=y_actual, Hist=rmse1step_r_auto[,3], MRP=rmse1step_m_auto[,2], RAW = rmse1step_r_auto[,2])
#filen <- paste(State,"_T2=Rolling",".RData", sep="")
#save.image(filen)


