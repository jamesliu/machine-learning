library(quantmod)
getSymbols("DDD")

avgP = function(symbol) {
  apply(HLC(symbol), 1, mean)  
}

vtable = function(symbol, margin = 0.05,  days = 5) {
  p = avgP(symbol)
  # initialize NA matrix
  r = matrix(NA, ncol = days, nrow = NROW(symbol))
  # calculate Vi
  for (x in 1:days) r[, x] = Next(Delt(p, k=x ), x)
  r
}

indicator = function(symbol, margin = 0.05, days = 5) {
  r = vtable(symbol, margin, days)
  v = apply(r[1: (NROW(symbol) - 0),], 1, function(y) sum(y[y> margin | y < -margin]))
  if (is.xts(symbol))
    xts(v, time(symbol[1: (NROW(symbol) - 0),]))
  else 
    v
  
}

myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("DDD.High", "DDD.Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("DDD.High", "DDD.Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("DDD.High", "DDD.Low")], x[, "DDD.Volume"])[,+ 2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("DDD.High", "DDD.Low", "DDD.Close")],+ x[, "DDD.Volume"])
mySAR <- function(x) SAR(x[, c("DDD.High", "DDD.Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,+ 1]

library(randomForest)

days = 5
margin = 0.05

pDDD = DDD[1: (NROW(DDD) - days),]

specifyModel(indicator(DDD, margin, days) ~ Delt(Cl(DDD),k=1:10))
data.model <- specifyModel(indicator(DDD, margin, days) ~ Delt(Cl(pDDD),k=1:10) + myATR(pDDD) + mySMI(pDDD) + myADX(pDDD) + myAroon(pDDD) +
                             myBB(pDDD) + myChaikinVol(pDDD) + myCLV(pDDD) + CMO(Cl(pDDD)) + EMA(Delt(Cl(pDDD))) + myEMV(pDDD) +
                             myVolat(pDDD) + myMACD(pDDD) + myMFI(pDDD) + RSI(Cl(pDDD)) + mySAR(pDDD) + runMean(Cl(pDDD)) + 
                             runSD(Cl(pDDD))
                            )
set.seed(1234)
rf <- buildModel(data.model,method=randomForest, training.per=c(start(DDD),index(DDD["2012-11-29"])), ntree=50, importance=T)