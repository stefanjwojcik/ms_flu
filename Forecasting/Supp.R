require(zoo)
require(lubridate)
smoothA1Process <- function(filen = NA, State = State) {
  
  Y <- data.frame(read.csv(filen , header = TRUE,stringsAsFactors = FALSE))
  ind <- which(colnames(Y) == State)
  Y <- Y[,c(1,ind)]
  Y <- na.omit(Y)
  # Interpolate Missing Dates
  
  Y$date <- as.Date(Y$date , format="%m/%d/%Y")
  alldates = seq(min(Y$date), max(Y$date), 1)
  dates0 = alldates[!(alldates %in% Y$date)]
  Y0 = data.frame(date = dates0, values = NA)
  colnames(Y0) = colnames(Y)
  Y = rbind(Y, Y0)
  Y = Y[order(Y$date),]
  Y[,2] = na.approx(zoo(Y[,2]))
  
  # Aggregate by Week and Year
  Y$week <- epiweek(Y$date)
  Y$year <- epiyear(Y$date)
  Y <- Y[-which(Y$week==53),]
  Y <- aggregate(as.formula(paste0(State,"~week+year")), FUN=mean, data=Y)
  #Y <- Y[seq(1,to=nrow(Y),by=7),]
  #return(Y)
  return(Y[1:(nrow(Y)),])
  
}


trainedModels <- function(T2, season, Ysub,method) {
  
    
  ######## ARIMA-X With Selected Queries And Forecasts  ##############################################################################
  
  Yt <- as.matrix(Ysub[1:T2,1])
  Xt <- as.matrix(Ysub[1:T2,2:ncol(Ysub)])
  mdl <- modelSelect(Yt, xreg = Xt, period = season, method = method)
  return(mdl)
  
}

nStepAheadAuto <- function(Ysub, mdlx, mdlhist, ahead, method, season=0, T2, incl_mean, incl_mean_hist) {
  
  ahead <- ahead
  T <- nrow(Ysub)
  yforex <- rep(0, length(c((T2 + ahead):T)))
  yforeh <- rep(0, length(c((T2 + ahead):T)))
  yforexonly <- rep(0, length(c((T2 + ahead):T)))

  require(forecast)
  for (hh in 1:length(c((T2 + ahead):T))) {
    
    
    print(hh)
    
    modelxlag <- try(Arima(ts(Ysub[hh:(T2 + hh - 1), 1],frequency=season), order = c(mdlx$p, mdlx$d, mdlx$q), seasonal = list(order = c(mdlx$sp, mdlx$sd, mdlx$sq), period = season), xreg = Ysub[hh:(T2 + hh - 1),2:ncol(Ysub)], method = method, include.mean = incl_mean))
    model     <- try(Arima(ts(Ysub[hh:(T2 + hh - 1), 1],frequency=season), order = c(mdlhist$p, mdlhist$d, mdlhist$q), seasonal = list(order = c(mdlhist$sp, mdlhist$sd, mdlhist$sq), period = season), method = method, include.mean = incl_mean_hist))
    
    if(ncol(Ysub)>2) {
      tempxlag <- predict(modelxlag, n.ahead = ahead, newxreg = data.frame(Xt1=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 2] , Xt2=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 3]))
    }
    if(ncol(Ysub)==2) {
      tempxlag <- predict(modelxlag, n.ahead = ahead, newxreg = data.frame(Xt1=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 2]))
    }
    temph <- predict(model, n.ahead = ahead)
    
    yforex[hh]  <- tempxlag$pred[ahead]
    yforeh[hh]  <- temph$pred[ahead]
    
    
  }
  Yte = as.matrix(Ysub[(T2+ahead):(T),1])
  rmseX = sqrt(mean((yforex - Yte) ^ 2))
  mapeX = mean(abs((yforex - Yte)/Yte))
  rmsehist = sqrt(mean((yforeh - Yte) ^ 2))
  mapehist = mean(abs((yforeh - Yte)/Yte))
  
  return(cbind(ytrue =Yte, yforex = yforex, yforeh = yforeh, rmseX=rmseX, rmsehist = rmsehist, mapeX = mapeX, mapehist = mapehist))
  
}


require(TSA)
nStepAhead <- function(Ysub, mdlx, mdlhist, ahead, method, season=0, T2) {
  
  ahead <- ahead
  T <- nrow(Ysub)
  yforex <- rep(0, length(c((T2 + ahead):T)))
  yforeh <- rep(0, length(c((T2 + ahead):T)))
  yforexonly <- rep(0, length(c((T2 + ahead):T)))
#  yforesparse <- rep(0, length(c((T2 + ahead):T)))
  
  ## BigVAR Model
  
#  require(BigVAR)
#  VARX=list()
#  VARX$k=1 # indicates that the first two series are modeled
#  VARX$s=1 # sets 2 as the maximal lag order for exogenous series
#  VARX$contemp = TRUE
#  T1=104

  
  for (hh in 1:length(c((T2 + ahead):T))) {
    
    
    print(hh)
    
    modelxlag <- try(arimax(Ysub[1:(T2 + hh - 1), 1], order = c(mdlx$p, mdlx$d, mdlx$q), seasonal = list(order = c(mdlx$sp, mdlx$sd, mdlx$sq), period = season), xreg = Ysub[1:(T2 + hh - 1),2:ncol(Ysub)], method = method, include.mean=F))
    model <- try(arimax(Ysub[1:(T2 + hh - 1), 1], order = c(mdlhist$p, mdlhist$d, mdlhist$q), seasonal = list(order = c(mdlhist$sp, mdlhist$sd, mdlhist$sq), period = season), method = method, include.mean=F))
    
 #   model_sparse <- constructModel(as.matrix(Ysub_mrp[,1]),p=52,struct="Lasso",gran=c(50,10),verbose=FALSE,VARX=VARX,T1=T1+hh-1,T2=T2+hh-1)
#    results <- cv.BigVAR(Model1)
    
    
    if(ncol(Ysub)>2) {
      tempxlag <- predict(modelxlag, n.ahead = ahead, newxreg = data.frame(Xt1=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 2] , Xt2=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 3]))
    }
    if(ncol(Ysub)==2) {
      tempxlag <- predict(modelxlag, n.ahead = ahead, newxreg = data.frame(Xt1=Ysub[c((T2 + hh):(T2 + hh + ahead-1)), 2]))
    }
    temph <- predict(model, n.ahead = ahead)
    
    yforex[hh]  <- tempxlag$pred[ahead]
    yforeh[hh]  <- temph$pred[ahead]
    
    
  }
  Yte = as.matrix(Ysub[(T2+ahead):(T),1])
  rmseX = sqrt(mean((yforex - Yte) ^ 2))
  mapeX = mean(abs((yforex - Yte)/Yte))
  rmsehist = sqrt(mean((yforeh - Yte) ^ 2))
  mapehist = mean(abs((yforeh - Yte)/Yte))
  
  return(cbind(ytrue =Yte, yforex = yforex, yforeh = yforeh, rmseX=rmseX, rmsehist = rmsehist, mapeX = mapeX, mapehist = mapehist))
  
}

merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}

