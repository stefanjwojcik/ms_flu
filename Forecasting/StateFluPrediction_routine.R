
StateResults <- function(start, del_ind, State, StateFile, logMRP,  method, season, T2, lag_m, lag_r) {

T1 <- 1  
source("Supp.R")
  
  
if(State != "NY") {
   Z <- data.frame(read.csv("Data/DE-NM-MI-DCFlu-2.csv" , header = TRUE,stringsAsFactors = FALSE)) # States
   ind <- which(Z$State == State)
   Z <- Z[ind,]
   Z <- Z[start:nrow(Z),]
   
} else {
   Z <- data.frame(read.csv("Data/NYFlu.csv" , header = TRUE,stringsAsFactors = FALSE))
}

Z$Week.Ending <- as.Date(Z$Week.Ending , format="%m/%d/%Y")
Z$week <- week(Z$Week.Ending)
Z$year <- year(Z$Week.Ending)
Z$wy <- apply(Z[,c(9,10)], 1, paste, collapse = "-")
#Z <- Z[-which(Z$week==53),]

if(State == "NM") {
 
  Z$metric <- na.approx(Z$Total)

} else {
  
  Z$metric <- na.approx(Z$Positive)
  
}

# Search Data Process and Merge with Flu Rates (match on existing weeks)
Ymrp <- smoothA1Process("Data/estimates_flu_states_3daywindowV7.csv", State)

#Yraw <- smoothA1Process(StateFile, State)
#Ydenom <- data.frame(read.csv("Data/SRPV_AllStates.csv" , header = TRUE,stringsAsFactors = FALSE)) # States
#weeks <- c()
#years <- c()
#for(i in 1:nrow(Ydenom)) {
  
#  if(nchar(Ydenom$WeekID[i]) == 6){
#    weeks <- c(weeks, substring(Ydenom$WeekID[i],2,2))
#    years <- c(years, substring(Ydenom$WeekID[i],3,6))
#  }
#  if(nchar(Ydenom$WeekID[i]) == 7){
#    weeks <- c(weeks, substring(Ydenom$WeekID[i],2,3))
#    years <- c(years, substring(Ydenom$WeekID[i],4,7))
    
#  }
  
#}
#Ydenom$week <- as.numeric(weeks)
#Ydenom$year <- as.numeric(years)
#Ydenom <- Ydenom[-which(Ydenom$week==53),]
#Ydenom$wy <- apply(Ydenom[,c(53,54)], 1, paste, collapse = "-")

Ymrp$wy   <- apply(Ymrp[,c(1,2)], 1, paste, collapse = "-")
#Yraw$wy   <- apply(Yraw[,c(1,2)], 1, paste, collapse = "-")


#Ydenom <- Ydenom[match(Z$wy , Ydenom$wy),]
#Yraw <- Yraw[match(Z$wy, Yraw$wy),]
Ymrp <- Ymrp[match(Z$wy, Ymrp$wy),]

Z <- Z[1:(nrow(Z)-del_ind),]
#Ydenom <- Ydenom[1:(nrow(Ydenom)-del_ind),]
Ymrp <- Ymrp[1:(nrow(Ymrp)-del_ind),]
#Yraw <- Yraw[1:(nrow(Yraw)-del_ind),]
#Yrr <- na.approx(Yraw[,3])/(na.approx(Ydenom[State]))
#Yr <- log(Yrr/(1-Yrr))

Yr <- read.csv(paste("Data/",State,"_normalized.csv",sep=""),header = T, stringsAsFactors = F)
Yr <- Yr[,State]
#stop("here")

Yf <- na.approx((Z$metric))

if(logMRP == 1) {
  Ym <- na.approx(log(Ymrp[,3]))
}
if(logMRP == 0) {
  Ym <- na.approx((Ymrp[,3]))
}
###


Ysub_mrp <- data.frame(cbind(Yf, Ym)) # Weighted
Ysub_raw <- data.frame(cbind(Yf, Yr)) # Raw

#T2 = ceiling(2*nrow(Ysub_mrp)/3)


# Adjust for Lags
if(lag_m==lag_r & lag_m > 0) {
  
  Ysub_mrp <- data.frame(cbind(Yf[(lag_m+1):length(Yf)], Ym[1:(length(Ym)-lag_m)])) # Weighted
  Ysub_raw <- data.frame(cbind(Yf[(lag_r+1):length(Yf)], Yr[1:(length(Yr)-lag_r)])) # Raw
  
}

## Auto-Arima
require(forecast)
mdl_hist_auto <- auto.arima(ts(Ysub_mrp[T1:T2,1],frequency=season),approximation=F, parallel = T,stepwise = F, seasonal = T, ic = "aic",allowdrift = F, allowmean = F,max.D=1)
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

mdl_mrp_auto <- auto.arima(ts(Ysub_mrp[T1:T2,1],frequency=season) ,approximation=F, xreg = Ysub_mrp[T1:T2,2],parallel = T,stepwise = F, seasonal = T, ic = "aic",allowdrift = F,allowmean = F, max.D=1)
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
} else {
  mdl_m_auto$sp <- 0
  mdl_m_auto$sd <- 0 #mdl_params[5]
  mdl_m_auto$sq <- 0
}

mdl_raw_auto <- auto.arima(ts(Ysub_raw[T1:T2,1],frequency=season) ,approximation=F, xreg = Ysub_raw[T1:T2,2], parallel = T, stepwise = F, seasonal = T, ic = "aic",allowdrift = F, allowmean = F, max.D = 1)
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
} else {
  mdl_r_auto$sp <- 0
  mdl_r_auto$sd <- 0 #mdl_params[5]
  mdl_r_auto$sq <- 0
}


# Compute Prediction Errors (Auto Arima)
method = method
mdlx <- mdl_m_auto
mdlh <- mdl_h_auto
mdlr <- mdl_r_auto

rmse1step_m_auto <- nStepAheadAuto(Ysub = Ysub_mrp, mdlx, mdlh, ahead=1, method = method, season=season, T2=T2, incl_mean = F, incl_mean_hist = F)
rmse2step_m_auto <- nStepAheadAuto(Ysub = Ysub_mrp, mdlx, mdlh, ahead=2, method = method, season=season, T2=T2, incl_mean = F, incl_mean_hist = F)

rmse1step_r_auto <- nStepAheadAuto(Ysub = Ysub_raw, mdlr, mdlh, ahead=1, method = method, season=season, T2=T2, incl_mean = F, incl_mean_hist = F)
rmse2step_r_auto <- nStepAheadAuto(Ysub = Ysub_raw, mdlr, mdlh, ahead=2, method = method, season=season, T2=T2, incl_mean = F, incl_mean_hist = F)

## Compute RMSE

y_actual <- Ysub_mrp[(T2+1):nrow(Ysub_mrp),1]

resultsdf  <- cbind.data.frame(Actual=y_actual, Hist=rmse1step_r_auto[,3], MRP=rmse1step_m_auto[,2], RAW = rmse1step_r_auto[,2])
results2df <- cbind.data.frame(Actual=y_actual[2:length(y_actual)], Hist=rmse2step_r_auto[,3], MRP=rmse2step_m_auto[,2], RAW = rmse2step_r_auto[,2])


mrp_acc  <- accuracy(rmse1step_m_auto[,2],y_actual)
hist_acc <- accuracy(rmse1step_m_auto[,3],y_actual)
raw_acc  <- accuracy(rmse1step_r_auto[,2],y_actual)

mrp_acc_2  <- accuracy(rmse2step_m_auto[,2],y_actual[2:length(y_actual)])
hist_acc_2 <- accuracy(rmse2step_m_auto[,3],y_actual[2:length(y_actual)])
raw_acc_2  <- accuracy(rmse2step_r_auto[,2],y_actual[2:length(y_actual)])

filen <- paste(State, ".csv", sep="")
write.csv(resultsdf,filen,row.names = F)
filen <- paste(State, "2.csv", sep="")
write.csv(results2df,filen,row.names = F)

## PLot Absolute Errors
T2 = ceiling(2*nrow(Ysub_mrp)/3)
Dates <- Z$Week.Ending
ahead <- 1
T2 = T2 + ahead-1
showlen <- 32
ypredm <- tail(rmse1step_m_auto[,2],showlen)
ypredx <- tail(rmse1step_r_auto[,2],showlen)
ypredh <- tail(rmse1step_r_auto[,3],showlen)
ytrue <-  tail(rmse1step_m_auto[,1],showlen)

df1 <- data.frame(cbind.data.frame(Prediction=ypredm, Signal = rep("mrp",length(ypredm)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df3 <- data.frame(cbind.data.frame(Prediction=ypredh, Signal = rep("history",length(ypredh)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df4 <- data.frame(cbind.data.frame(Prediction=ytrue, Signal = rep("actual",length(ytrue)), Week.Ending=tail(Dates,showlen)), stringsAsFactors = F)
require(reshape2)
df <- rbind.data.frame(df1,df3)
df_act <- rbind.data.frame(df4,df4)
df$Error <- abs(as.numeric(df$Prediction)-as.numeric(df_act$Prediction))
df$Year <- year(df$Week.Ending)
df$Week.Ending <- as.Date(df$Week.Ending,format="%Y-%m-%d")
require(ggplot2)

cbbPalette <- c("#D55E00","#000000")

p <- ggplot(data=df, aes(x=Week.Ending, y=Error, group = Signal, colour = Signal)) +  
  geom_line(aes(linetype=Signal),size=1.0) + ggtitle(paste(State,"(h=1)",sep=" "))
p <- p + scale_color_manual(values=cbbPalette) + scale_x_date() + xlab("Date") + ylab("Absolute Error")
#p <- p + scale_color_manual(values=cbbPalette) + xlab("Date")


p <- p + theme(axis.text=element_text(size=12,face="bold"),
               axis.title=element_text(size=16,face="bold"), plot.title = element_text(size=16,face="bold"), legend.text=element_text(size=12,face="bold")) 
print(p)

ggsave(paste(State,"_h_1_Abs_error.pdf",sep=""), width = 7, height = 7)


return(list(mrp=mrp_acc, mrp_2=mrp_acc_2, hist=hist_acc, hist_2=hist_acc_2, raw=raw_acc, raw_2=raw_acc_2))


}




