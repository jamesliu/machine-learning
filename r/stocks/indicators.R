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

data.model <- specifyModel(indicator(DDD) ~ Delt(Cl(DDD),k=1:10) + myATR(DDD) + mySMI(DDD) + myADX(DDD) + myAroon(DDD) +
                             myBB(DDD) + myChaikinVol(DDD) + myCLV(DDD) + CMO(Cl(DDD)) + EMA(Delt(Cl(DDD))) + myEMV(DDD) +
                             myVolat(DDD) + myMACD(DDD) + myMFI(DDD) + RSI(Cl(DDD)) + mySAR(DDD) + runMean(Cl(DDD)) + 
                             runSD(Cl(DDD))
                            )
set.seed(1234)
rf <- buildModel(data.model,method="randomForest", training.per=c(start(DDD),index(DDD["2012-11-29"])), ntree=50, importance=T)

# Check the importance of the features
varImpPlot(rf@fitted.model, type = 1)

imp = importance(rf@fitted.model, type = 1)
rownames(imp)[imp > 3.5]

# Extract important features

data.model <- specifyModel(indicator(DDD) ~ Delt(Cl(DDD),k=6) + myATR(DDD) + mySMI(DDD) + myADX(DDD) + myAroon(DDD) +
                             myVolat(DDD) + myMACD(DDD) + myMFI(DDD) + mySAR(DDD) + runMean(Cl(DDD)) + 
                             runSD(Cl(DDD)))

# Transform xts 

Tdata.train <- as.data.frame(modelData(data.model, data.window=c('2011-06-01','2012-05-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c( '2012-06-01','2012-11-29'))))
Tform <- as.formula('indicator.DDD ~ .')