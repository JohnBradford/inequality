fitData.sweep <- function(v0=c(.6, .55, .5, .45, .4, .35), step=.001, verbose=TRUE, yrs=1986, 
                           wtsVar=NULL, fits=5, takeMean=FALSE,
                           args=list(df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
                          df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=yrs)) {
##### START HERE ############
source("dataFit.R")
v <- v0  # omega parameter values; percentage lost by loser in interaction
args_n <- c(list(values=v0), args)
fresults <- do.call(fit.data, args_n)
bestGlobal <- as.numeric(fresults[1])
bestGlobalpar <- v
bestGlobalparMean <- v
bestLocalpar <- v
bestLocalfits <- fresults[[2]]
bestLocalfitsMean <- bestLocalfits
output <- data.frame(sumSq=rep(NA, length(v)*1000), bestGlobal=bestGlobal)
output$par <- list(v0)
output$bestGlobalpar <- list(v0)
output$bestLocalpar <- list(v0)
output$bestGlobalMean <- list(v0)

t <- 1
vec_order <- length(v):1
if(!is.null(wtsVar)){
  vec_order <- order(wtsVar, decreasing=TRUE)}   #if no other info is provided
  #sweeping in order of weights leads to inferior results

sweep <- 1 ##sweep number 1
for(l in 1:2) {
  if(verbose==TRUE){if(sweep==1){message("SWEEP 1........")} 
  else{message("SWEEP 2........")}}

  ##PRIMARY LOOP ACROSS INDEXES
for(k in vec_order){ #for each vector item k, ....
  if(verbose==TRUE){message(paste("fitting index ", k, sep=""))}
  if(sweep==1){
    numFits <- fits
    #best_vk <- v[k]
    bestLocalfits[k] <- fresults[[2]][k]  ##least sum of squared residuals obtained for omega[k]
    STEP <- step * 10   #broader inquiry first
    beginSweep <- v0[k]
    endSweep <- 0
    #message(paste("endSweep=", endSweep, sep=""))
   # if(!is.null(wtsVar)){endSweep <- (length(v) - k) * STEP }
    #ensuring when sweeping down, v[k+1] do not reduce below zero  
    #message(paste("endSweep=", endSweep, sep=""))
      #e.g., if v[3] has largest weight, it can only be reduced to
      #(6-3)*.1, or .3, because at that point, v[6] would already be at zero
  } else {
    numFits <- fits
   # best_vk <- bestGlobalpar[k]
    STEP <- step
    #bestLocal <- bestLocalfits[k]  
    beginSweep <- bestGlobalpar[k] + STEP*10
    endSweep  <- max(bestGlobalpar[k] - STEP*10, 0)
    #if(!is.null(wtsVar)){endSweep <- (length(v) - k) * STEP*10}
  }
  if(beginSweep<=endSweep){beginSweep <- beginSweep + (endSweep - beginSweep) + step * 10}
  ##reduce last (smallest) omega all the way, then pick best, then do second to last, etc..
  #vKmax <- max(v[k+1], 0.01, na.rm=T)
  #if(v[k]<=vKmax){v[k]<-v[k]+.1} #in case beginning and end of sequence below are the same
  
  #SECONDARY LOOP - FITTING EACH INDEX
  if(verbose==TRUE){message(paste("beginSweep=", beginSweep, ". endSweep=", endSweep, sep=""))}
  for(newValue in seq(beginSweep, endSweep, by=-STEP)){ ##countdown of values, decreasing from max, in column k (6 --> 1)
    #message(paste("beginSweep=", beginSweep, " . endSweep =", endSweep))

    q <- replicate(numFits, do.call(fit.data, args_n))
    output$sumSq[t] <- mean(as.numeric(q[1,]))
    output$par[t] <- list(v)
    #row1 = global sum of squared residuals, cols = for each iteration
    #row2 = vector of length k, sum of squared residuals for each vector item k (omega)
    #q[[2,3]][2]  #row2, column3, item 2 in the vector
        
    #####ITERATING N=FITS, calculating averages across trials
    meanLocalSumSqrs <- as.numeric()
        for(i in 1:length(v)){
          total <- 0
          for(f in 1:numFits){
            total <- total + q[[2,f]][i]
          }
          kMean <- total/numFits
          meanLocalSumSqrs[i] <- kMean
        }
        

        
        #GLOBAL - total sum of squares #if sweeping in random index order, update locals for all indexes
    if(output$sumSq[t] < bestGlobal) {
      bestGlobal <- output$sumSq[t]
      bestGlobalpar[k] <- v[k]
      bestGlobalparMean[k] <- mean(c(bestGlobalparMean[k], bestGlobalpar[k]))
    if(!is.null(wtsVar)) {for(vIndex in vec_order) {
      bestGlobalpar[vIndex] <- v[vIndex]
      bestGlobalparMean[vIndex] <- mean(c(bestGlobalparMean[vIndex], bestGlobalpar[vIndex]))
      }}
      }
    
    output$bestGlobalpar[t] <- list(bestGlobalpar)
    output$bestGlobal[t] <- bestGlobal
    output$bestGlobalMean[t] <- list(bestGlobalparMean)
    
    #LOCAL improvement, whether sum of squares reduces for this class, for each v
    if(!is.null(wtsVar)) {
      for(vIndex in vec_order) { #if sweeping in random index order, update locals for all indexes
        if(meanLocalSumSqrs[vIndex] < bestLocalfits[vIndex]) {
          bestLocalpar[vIndex] <- v[vIndex]  #update to new value of v[k]
          bestLocalfits[vIndex] <- meanLocalSumSqrs[vIndex] #value of smallest squared residuals for this class
        }
      }} else {
        if(meanLocalSumSqrs[k]< bestLocalfits[k]) {
      bestLocalpar[k] <- v[k]  #update to new value of v[k]
      bestLocalfits[k] <- meanLocalSumSqrs[k] #value of smallest squared residuals for this class
    }}

    output$bestLocalpar[t] <- list(bestLocalpar)

    v[k] <- newValue
    #message(paste("newValue =", newValue, sep=""))
    
    for(m in k:length(v)) { #when using wts, ensure constraint of order is met, k1 > k2 > k3 ...
      if(m+1 <= length(v) & v[m]<=v[m+1]){
        v[m+1] <- v[m]-STEP
        if(v[m+1] < 0){v[m+1] <- 0} ##setting to zero
      }
    }
    
    if(verbose==TRUE){message(paste("t=", t, " sum sqrs = ", round(output[t,1], 4), ". best global = ", 
                                    round(output[t,2], 4), ". par = ", lapply(output[t,3], round, 3)))}
    
    args_n <- c(list(values=v), args)
    t <- t + 1
  } ##end of fitting values for index K
  
  
  v <- bestGlobalpar
  if(takeMean==TRUE){v <- bestGlobalparMean}
  ##Also, have option to reset according to local minima
  # for(j in 1:length(v)){v[j] <- bestLocalfits[j]}
  args_n <- c(list(values=v), args)
  t <- t + 1
}
sweep <- 2 #sweep #2
}
return(list(df_out=output, Global=bestGlobal, Global_par=bestGlobalpar, Localpar=bestLocalpar, bestLocalfits=bestLocalfits))
}
