fitData.anneal <- function(v0=c(.5, .45, .40, .35, .30, .25), step=.01, verbose=TRUE, yrs=1986,
                   args=list(df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
                          df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=yrs)) {
##### START HERE ############
source("dataFit.R")
v <- v0
args_n <- c(list(values=v0), args)
fresults <- do.call(fit.data, args_n)
bestGlobal <- as.numeric(results[1])
bestGlobalpar <- v
bestLocalpar <- v
output <- data.frame(sumSq=rep(NA, 1000), bestGlobal=bestGlobal)
output$par <- list(v0)
output$bestGlobalpar <- list(v0)
output$bestLocalpar <- list(v0)

t <- 1
for(k in length(v):1){
  best_vk <- v[k]
  bestLocal <- fresults[[2]][k]
  ##reduce last (smallest) omega all the way, then pick best, then do second to last, etc..
  vKmax <- max(v[k+1], 0.01, na.rm=T)
  if(v[k]<=vKmax){v[k]<-v[k]+.1} #in case beginning and end of sequence below are the same
  for(newValue in seq(v[k], vKmax, by=-step)){ ##countdown of values, decreasing from max, in column k (6 --> 1)
    
        results <- do.call(fit.data, args_n)#############

    output$par[t] <- list(v)
    output$sumSq[t] <- as.numeric(results[1])

    #GLOBAL - total sum of squares
    if(output$sumSq[t] < bestGlobal) {
      bestGlobal <- output$sumSq[t]
      bestGlobalpar <- v
      best_vk <- v[k]  ###########updating best vector item if it reduces overall/global sq. residuals
      
    }
    
    output$bestGlobalpar[t] <- list(bestGlobalpar)
    output$bestGlobal[t] <- bestGlobal
    
    #local improvement, whether sum of squares reduces for this class
    if(results[[2]][k] < bestLocal) {
      bestLocal <- results[[2]][k] #value of smallest squared residuals for this class
      bestLocalpar[k] <- v[k]  #update to new value of v[k]
    }
    
    output$bestLocalpar[t] <- list(bestLocalpar)

    v[k] <- newValue
    if(verbose==TRUE){message(paste("t=", t, " sum sqrs = ", round(output[t,1], 4), ". best global = ", 
                                    round(output[t,2], 4), ". par = ", lapply(output[t,3], round, 3)))}
    
    args_n <- c(list(values=v), args)
    t <- t + 1
  } 
  v[k] <- best_vk
  args_n <- c(list(values=v), args)
  t <- t + 1
}
return(list(df_out=output, Global=bestGlobal, Global_par=bestGlobalpar, Local=bestLocalpar))
}
