

# compare
getSymbols(c("DDD", "SSYS"))
MMM = merge(DDD, SSYS)
candleChart(MMM)

# add TA
candleChart(DDD)

# volatility
addBBands()
addATR()
addEnvelope()

# momentum
# Commodity Channel Index  CCI
addCCI()
# Chande Momentum Oscillator  CMO	
addCMO()
# Detrended Price Oscillator	DPO	
addDPO()
# momentum	
addMomentum()
# Rate of Change	ROC	
addROC()
# Relative Strength Indicator	RSI	
addRSI()
# Stocastic Momentum Index	SMI	
addSMI()
# Williams %R	WPR	
addWPR()


# volume
addCMF()
#addVo()