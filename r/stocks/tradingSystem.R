###################################################
### The Trading System
###################################################
data <- tail(Tdata.train,240)
results <- list()
for(name in namesBest) {
  sys <- getVariant(name,fullResults)
  results[[name]] <- runLearner(sys,Tform,data,Tdata.eval)
}
results <- t(as.data.frame(results))


results[,c('Ret','RetOverBH','MaxDD','SharpeRatio','NTrades','PercProf')]


getVariant('grow.nnetR.v12',fullResults)


model <- learner('MC.nnetR',list(maxit=750,linout=T,trace=F,size=10,decay=0.001))
preds <- growingWindowTest(model,Tform,data,Tdata.eval,relearn.step=120)
signals <- factor(preds,levels=1:3,labels=c('s','h','b'))
date <- rownames(Tdata.eval)[1]
market <- DDD[paste(date,"/",sep='')][1:length(signals),]
trade.res <- trading.simulator(market,signals,policy.func='pol2')


plot(trade.res,market,theme='white',name='DDD - final test')


library(PerformanceAnalytics)
rets <- Return.calculate(trade.res@trading$Equity)


chart.CumReturns(rets,main='Cumulative returns of the strategy',ylab='returns')


yearlyReturn(trade.res@trading$Equity)


plot(100*yearlyReturn(trade.res@trading$Equity),
     main='Yearly percentage returns of the trading system')
abline(h=0,lty=2)


table.CalendarReturns(rets)


table.DownsideRisk(rets)


