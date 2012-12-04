library(qmao)
library(rjson)
ec <- getEarningsCalendar(from="2011-01-01", to="2012-07-01") #this may take a while
s <- unique(ec$Symbol)

sink("symbols.txt")
cat(toJSON(s))
sink()
file.show("symbols.txt")

