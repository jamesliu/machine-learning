library(earth)
e <- earth(Tform, Tdata.train[1:100, ])
e.preds <- predict(e, Tdata.train[101:200, ])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[101:200, "indicator.DDD"],+ 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)