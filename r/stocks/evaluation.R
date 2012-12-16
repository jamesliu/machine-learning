require(DMwR)


monteCarlo2 =function (learner, data.set, mcSet, itsInfo = F, verbose = T) 
{
  show(mcSet)

  itsI <- results <- NULL
  n <- NROW(data.set@data)
  train.size <- if (mcSet@mcTrain < 1) 
    as.integer(n * mcSet@mcTrain)
  else mcSet@mcTrain
  test.size <- if (mcSet@mcTest < 1) 
    as.integer(n * mcSet@mcTest)
  else mcSet@mcTest
  if (n - test.size + 1 <= train.size + 1) 
    stop("monteCarlo:: Invalid train/test sizes.")
  set.seed(mcSet@mcSeed)
  selection.range <- (train.size + 1):(n - test.size + 1)
  starting.points <- sort(sample(selection.range, mcSet@mcReps))

  for (it in seq(along = starting.points)) {
    start <- starting.points[it]
    if (verbose) 
      cat("Repetition ", it, "\n\t start test = ", start, 
          "; test size = ", test.size, "\n")
    itDS <- dataset(data.set@formula, data.set@data[(start - 
                                                       train.size):(start + test.size - 1), ])
    # e.g. runLearner will call SingleModel in montecarlo.R => call eval.stats in evaluation.R => trading.simulator
    rep.res <- runLearner(learner, data.set@formula, data.set@data[(start - 
                                                                      train.size):(start - 1), ], data.set@data[start:(start + 
                                                                                                                         test.size - 1), ])
    if (itsInfo && !is.null(tmp <- attr(rep.res, "itInfo"))) 
      itsI <- c(itsI, tmp)
    results <- rbind(results, rep.res)
  }
  if (verbose) 
    cat("\n")
  rownames(results) <- 1:nrow(results)
  set.seed(prod(as.integer(unlist(strsplit(strsplit(date(), 
                                                    " ")[[1]][4], ":")))))
  if (itsInfo) 
    return(structure(mcRun(learner, as(data.set, "task"), 
                           mcSet, results), itsInfo = itsI))
  else return(mcRun(learner, as(data.set, "task"), mcSet, results))
}

experimentalComparison2 = function (datasets, systems, setts) 
{
  require(abind, quietly = T)
  if (is.null(names(systems))) 
    names(systems) <- paste("var", 1:length(systems), sep = ".")
  results <- compExp(systems, lapply(datasets, function(x) as(x, 
                                                              "task")), setts, array())
  r <- NULL
  cat("\n\n##### ", switch(class(setts), cvSettings = "CROSS VALIDATION", 
                           hldSettings = "HOLD OUT", mcSettings = "MONTE CARLO", 
                           bootSettings = "BOOTSTRAP", loocvSettings = "LOOCV", 
  ), " EXPERIMENTAL COMPARISON #####")
  for (d in 1:length(datasets)) {
    cat("\n\n** DATASET ::", datasets[[d]]@name)
    rr <- NULL
    for (s in 1:length(systems)) {
      cat("\n\n++ LEARNER ::", systems[[s]]@func, " variant -> ", 
          names(systems)[s], "\n")
      
      # If class(setts) == mcSettings, monteCarlo function will be called
      
      var.res <- do.call(switch(class(setts), cvSettings = "crossValidation", 
                                hldSettings = "holdOut", bootSettings = "bootstrap", 
                                mcSettings = "monteCarlo2", loocvSettings = "loocv"), 
                         list(systems[[s]], datasets[[d]], setts))
      rr <- abind(rr, var.res@foldResults, along = 3)
    }
    r <- abind(r, rr, along = 4)
  }
  results@foldResults <- r
  dimnames(results@foldResults)[3:4] <- list(names(systems), 
                                             unlist(lapply(datasets, function(x) x@name)))
  results
}
  
eval.stats <- function(form,train,test,preds,b.t=0.1,s.t=-0.1,...) {
  # Signals evaluation
  
  tgtName <- all.vars(form)[1]
  test[,tgtName] <- trading.signals(test[,tgtName],b.t,s.t)
  st <- sigs.PR(preds,test[,tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),
                     c('s','b','sb'),sep='.')
  
  # Trading evaluation

  date <- rownames(test)[1]
  market <- ThisTicker[paste(date,"/",sep='')][1:length(preds),]
  trade.res <- trading.simulator(market,preds,...)

  c(st,tradingEvaluation(trade.res))
}

pol1 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.025,max.loss=0.05,hold.time=10)

pol2 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.05,max.loss=0.05,hold.time=20)

pol3 <- function(signals,market,op,money)
  policy.2(signals,market,op,money,
           bet=0.5,exp.prof=0.05,max.loss=0.05)


# The list of learners we will use
TODO <- c('svmR','svmC','earth','nnetR','nnetC')

# The data sets used in the comparison
DSs <- list(dataset(Tform,Tdata.train,'ThisTicker'))

# Monte Carlo (MC) settings used
MCsetts <- mcSettings(2,     # 10 repetitions of the MC exps
                      240,   # ~ half year for training
                      120,   # ~ 2 months for testing
                      1234)   # random number generator seed

# Variants to try for all learners
VARS <- list()
VARS$svmR   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$svmC   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetR  <- list(linout=T,maxit=750,size=c(5,10),
                    decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))
VARS$nnetC  <- list(maxit=750,size=c(5,10),decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))

# main loop
# do.call and variants in DMwR
for(td in TODO) {
  assign(td,
         experimentalComparison2(
           DSs,         
           c(
             do.call('variants',
                     c(list('singleModel',learner=td),VARS[[td]],
                       varsRootName=paste('single',td,sep='.'))),
             do.call('variants',
                     c(list('slide',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('slide',td,sep='.'))),
             do.call('variants',
                     c(list('grow',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('grow',td,sep='.')))
             ),
            MCsetts)
         )
  # save the results
  save(list=td,file=paste(td,'Rdata',sep='.'))
}

