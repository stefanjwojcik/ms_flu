rm(list=ls()) 
setwd("C:/Users/avbijral/Desktop/Disease/FluForecasting/")



source("modelSelection.R")
source("Supp.R")
require(zoo)


## Flu Rates
start = 1
State = "US"
Z <- data.frame(read.csv("Data/USFlu.csv" , header = TRUE,stringsAsFactors = FALSE))
Z <- Z[start:nrow(Z),]
Yf <- na.approx((Z$Total))

T1 = 1
T2 = 104+52
ahead = 1
Dates <- Z$Week
load('US_4_14_2020.RData')

T2 = T2 + ahead-1
showlen <- 32
ypredm <- tail(rmse2step_m_auto$yforex,showlen)
ypredmL <- tail(rmse2step_m_auto$yforexlower,showlen)
ypredmU <- tail(rmse2step_m_auto$yforexupper,showlen)

ypredr <- tail(rmse2step_r_auto$yforex,showlen)
ypredrL <- tail(rmse2step_r_auto$yforexlower,showlen)
ypredrU <- tail(rmse2step_m_auto$yforexupper,showlen)


ypredh <- tail(rmse2step_m_auto$yforeh,showlen)
ypredhL <- tail(rmse2step_m_auto$yforehlower,showlen)
ypredhU <- tail(rmse2step_m_auto$yforehupper,showlen)

ytrue <-  tail(Yf,showlen)

df1 <- data.frame(cbind(Prediction=ypredm, Signal = rep("SARIMA-MRP",length(ypredm)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df1U <- data.frame(cbind(Prediction=ypredmU, Upper = rep("U",length(ypredm)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df1L <- data.frame(cbind(Prediction=ypredmL, Lower = rep("L",length(ypredm)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)

#df2 <- data.frame(cbind(Prediction=ypredr, Signal = rep("raw",length(ypredr)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)

df3 <- data.frame(cbind(Prediction=ypredh, Signal = rep("SARIMA-HIST",length(ypredh)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df3U <- data.frame(cbind(Prediction=ypredhU, Upper = rep("U",length(ypredh)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)
df3L <- data.frame(cbind(Prediction=ypredhL, Lower = rep("L",length(ypredh)), Week.Ending=tail(Dates,showlen)),stringsAsFactors = F)

df4 <- data.frame(cbind(Prediction=ytrue, Signal = rep("OBSERVED",length(ytrue)), Week.Ending=tail(Dates,showlen)), stringsAsFactors = F)


require(reshape2)
df <- rbind(cbind(df1, lower = df1L$Prediction, upper = df1U$Prediction, stringsAsFactors=F) ,cbind(df3, lower = df3L$Prediction, upper = df3U$Prediction, stringsAsFactors=F),
            cbind(df4, lower = df4$Prediction, upper = df4$Prediction, stringsAsFactors=F), stringsAsFactors=F)

df$Prediction <- as.numeric(df$Prediction)
df$lower <- as.numeric(df$lower)
df$upper <- as.numeric(df$upper)
            

#df$Year <- as.Date(strptime(df$Week.Ending,"%m/%d/%Y"),format="%m/%d/%Y")
df$Year <- sapply(df$Week.Ending, function(x) substring(x,1,4))
df$Week <- sapply(df$Week.Ending, function(x) if(nchar(x)==6) paste("0",substring(x,6,nchar(x)),sep="") else substring(x,6,nchar(x)))
df$Date <- as.Date(paste(df$Year, df$Week, 7, sep="-"), format= "%Y-%U-%u")
df$Signal <- as.factor(df$Signal)

require(ggplot2)
require(RColorBrewer)
require(ggthemes)
require(GISTools)
dev.off(dev.list()["RStudioGD"])

p <-ggplot(data=df, aes(x=Date, y=Prediction, group = Signal, fill = Signal)) + geom_line(aes(linetype=Signal),size=0.8) 
p <- p +  ggtitle("National ILI Predictions vs Actual (2017) (h = 2 Step Ahead)")
p<-p+geom_ribbon(aes(ymin=df$lower, ymax=df$upper, fill = Signal, color = Signal), alpha=0.2)#+ scale_fill_manual(values=c( "#FFF7FB", "#ECE2F0", "#D0D1E6"))
p <- p + scale_fill_manual(values=brewer.pal(3, "Set1")) + scale_x_date() + xlab('Date')
#p <- p +  scale_x_date() + xlab('Date')
p <- p  + xlab('Date')
p <- p + theme(axis.text=element_text(size=16,face="bold"),
  axis.title=element_text(size=16,face="bold"), plot.title = element_text(size=16,face="bold"), legend.text=element_text(size=16,face="bold"),legend.position=c(0.15,0.75)) 
p


ggsave("US_Predictions_h=2.pdf", width = 7, height = 7)



