set.seed(1234)
library(nnet)

unscale = function (vals, norm.data, col.ids) 
{
  cols <- if (missing(col.ids)) 
    1:NCOL(vals)
  else col.ids
  if (length(cols) != NCOL(vals)) 
    stop("Incorrect dimension of data to unscale.")
  centers <- attr(norm.data, "scaled:center")[cols]
  scales <- attr(norm.data, "scaled:scale")[cols]
  unvals <- scale(vals, center = (-centers/scales), scale = 1/scales)
  attr(unvals, "scaled:center") <- attr(unvals, "scaled:scale") <- NULL
  unvals
}

norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:100, ], size = 10, decay = 0.01, maxit = 1000, linout = T, trace = F)
norm.preds <- predict(nn, norm.data[101:200, ])
preds <- unscale(norm.preds, norm.data)

sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[101:200, "indicator.DDD"], 0.1, -0.1)
sigs.PR(sigs.nn, true.sigs)

signals <- trading.signals(Tdata.train[, "indicator.DDD"], 0.1, -0.1)
norm.data <- data.frame(signals = signals, scale(Tdata.train[, -1]))
nn <- nnet(signals ~ ., norm.data[1:100, ], size = 10, decay = 0.01, maxit = 1000, trace = F)
preds <- predict(nn, norm.data[101:200, ], type = "class")
sigs.PR(preds, norm.data[101:200, 1])
