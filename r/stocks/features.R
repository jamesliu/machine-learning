library(quantmod)
getSymbols("DDD")
names(DDD) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")

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