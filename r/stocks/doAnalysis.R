

load("svmR.Rdata")
load("svmC.Rdata")
load("earth.Rdata")
load("nnetR.Rdata")
load("nnetC.Rdata")

tgtStats <- c('prec.sb','Ret','PercProf',
              'MaxDD','SharpeRatio')
allSysRes <- join(subset(svmR,stats=tgtStats),
                  subset(svmC,stats=tgtStats),
                  subset(nnetR,stats=tgtStats),
                  subset(nnetC,stats=tgtStats),
                  subset(earth,stats=tgtStats),
                  by = 'variants')
rankSystems(allSysRes,5,maxs=c(T,T,T,F,T))

summary(subset(svmC,
               stats=c('Ret','RetOverBH','PercProf','NTrades'),
               vars=c('slide.svmC.v5','slide.svmC.v6')))

fullResults <- join(svmR,svmC,earth,nnetC,nnetR,by='variants')
nt <- statScores(fullResults,'NTrades')[[1]]
rt <- statScores(fullResults,'Ret')[[1]]
pp <- statScores(fullResults,'PercProf')[[1]]
s1 <- names(nt)[which(nt > 20)]
s2 <- names(rt)[which(rt > 0.5)]
s3 <- names(pp)[which(pp > 40)]
namesBest <- intersect(intersect(s1,s2),s3)

compAnalysis(subset(fullResults,
                    stats=tgtStats,
                    vars=namesBest))

plot(subset(fullResults,
            stats=c('Ret','PercProf','MaxDD'),
            vars=namesBest))

# Get the information from specfic trading system
getVariant('single.nnetR.v12',nnetR)
