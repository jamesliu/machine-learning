require("DMwR")
# Train and test
start <- 1
len.tr <- 100
len.ts <- 50
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
# getting the quotes for the testing period

date <- rownames(Tdata.train[start+len.tr,])
market <- DDD[paste(date, '/', sep='')][1:len.ts]
colnames(market) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
# learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
# now using the simulated trader
t1 <- trading.simulator(market,sig,'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))

t1
summary(t1)
tradingEvaluation(t1)

plot(t1, market, theme = "white", name = "DDD")