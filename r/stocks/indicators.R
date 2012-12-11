library(quantmod)

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
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,+ 2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")],+ x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,+ 1]
