library(quantmod)
getSymbols("AMZN")
ThisTicker = AMZN
names(ThisTicker) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")

library(randomForest)

data.model <- specifyModel(indicator(ThisTicker) ~ Delt(Cl(ThisTicker),k=1:10) + myATR(ThisTicker) + mySMI(ThisTicker) + myADX(ThisTicker) + myAroon(ThisTicker) +
                             myBB(ThisTicker) + myChaikinVol(ThisTicker) + myCLV(ThisTicker) + CMO(Cl(ThisTicker)) + EMA(Delt(Cl(ThisTicker))) + myEMV(ThisTicker) +
                             myVolat(ThisTicker) + myMACD(ThisTicker) + myMFI(ThisTicker) + RSI(Cl(ThisTicker)) + mySAR(ThisTicker) + runMean(Cl(ThisTicker)) + 
                             runSD(Cl(ThisTicker))
)
set.seed(1234)
rf <- buildModel(data.model,method="randomForest", training.per=c(start(ThisTicker),index(ThisTicker["2012-11-29"])), ntree=50, importance=T)

# Check the importance of the features
varImpPlot(rf@fitted.model, type = 1)

imp = importance(rf@fitted.model, type = 1)
rownames(imp)[imp > 3.5]

# Extract important features

data.model <- specifyModel(indicator(ThisTicker) ~ Delt(Cl(ThisTicker),k=6) + myATR(ThisTicker) + mySMI(ThisTicker) + myADX(ThisTicker) + myAroon(ThisTicker) +
                             myVolat(ThisTicker) + myMACD(ThisTicker) + myMFI(ThisTicker) + mySAR(ThisTicker) + runMean(Cl(ThisTicker)) + 
                             runSD(Cl(ThisTicker)))

# Transform xts 

Tdata.train <- as.data.frame(modelData(data.model, data.window=c('2009-06-01','2012-05-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c( '2011-06-01','2012-11-29'))))
Tform <- as.formula('indicator.ThisTicker ~ .')