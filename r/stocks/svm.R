library(e1071)
sv <- svm(Tform, Tdata.train[1:100, ], gamma = 0.001, cost = 100)
s.preds <- predict(sv, Tdata.train[101:200, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[101:200, "indicator.DDD"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)


library(kernlab)
data <- cbind(signals = signals, Tdata.train[, -1])
ksv <- ksvm(signals ~ ., data[1:100, ], C = 10)

ks.preds <- predict(ksv, data[101:200, ])
sigs.PR(ks.preds, data[101:200, 1])