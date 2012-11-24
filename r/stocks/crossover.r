
signals = cbind(Cl(DDD), runMin(DDD, 30))
signals = cbind(signals, signals[,1] - signals[,2])
signals = cbind (signals,diff(sign(signals[,3])))
Cross_Date = rownames(as.matrix(signals))[which (signals[,4] != 0)]
addTA(signals[Cross_Date,1], on=1, type = "p", col=5, pch=24)
