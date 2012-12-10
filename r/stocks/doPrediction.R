# Set up environment
setwd("~/github/machine-learning/r/stocks/")

# Removing any extra objects from my workspace
rm(list=ls())

# functions
source("montecarlo.R")
source("policy.R")

# load and clean
source("indicators.R")


source("evaluation.R")