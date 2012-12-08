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
  market <- DDD[paste(date,"/",sep='')][1:length(preds),]
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
DSs <- list(dataset(Tform,Tdata.train,'DDD'))

# Monte Carlo (MC) settings used
MCsetts <- mcSettings(20,     # 20 repetitions of the MC exps
                      124,   # ~ 1 years for training
                      62,   # ~ 0.5 years for testing
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
for(td in TODO) {
  assign(td,
         experimentalComparison(
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

