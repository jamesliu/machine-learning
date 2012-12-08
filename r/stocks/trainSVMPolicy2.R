start <- 70
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
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))

summary(t2)
tradingEvaluation(t2)

plot(t2, market, theme = "white", name = "DDD")
