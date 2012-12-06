policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05)
  {
     d <- NROW(market) # this is the ID of today
     orders <- NULL
     nOs <- NROW(opened.pos)
     # nothing to do!
     if (!nOs && signals[d] == "h") return(orders)
     
     # First lets check if we can open new positions
     # i) long positions
     if (signals[d] == "b" && !nOs) {
       quant <- round(bet*money/market[d, 'Close'],0)
       if (quant > 0)
         orders <- rbind(orders,
                         data.frame(order=c(1,-1,-1),order.type=c(1,2,3),
                                    val = c(quant,
                                            market[d, "Close"]*(1+exp.prof),
                                            market[d, "Close"]*(1-max.loss)
                                            ),
                                    action = c("open", "close", "close"),
                                    posID = c(NA,NA,NA)
                                    )
                         )
       
       # ii) short positions
       } else if (signals[d] == "s" && !nOs) {
         # this is the nr of stocks we already need to buy
         # because of currently opened short positions
         need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                                    "N.stocks"])*market[d, "Close"]
         quant <- round(bet*(money-need2buy)/market[d, "Close"], 0)
         if (quant > 0)
           orders <- rbind(orders,
                           data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                      val = c(quant,
                                              market[d, "Close"]*(1-exp.prof),
                                              market[d, "Close"]*(1+max.loss)
                                              ),
                                      
                                      action = c("open", "close", "close"),
                                      posID = c(NA,NA,NA)
                                      )
                           )
         }
     # Now lets check if we need to close positions
     # because their holding time is over
     if (nOs)
       for(i in 1:nOs) {
         if (d - opened.pos[i, "Odate"] >= hold.time)
           orders <- rbind(orders,
                           data.frame(order=-opened.pos[i, "pos.type"],
                                      order.type=1,
                                      val = NA,
                                      action = "close",
                                      posID = rownames(opened.pos)[i]
                                      )
                           )
       }
     orders
}