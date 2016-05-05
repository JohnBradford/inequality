#Inequality Process: 
#One Parameter Inequality Process (OPIP), and 
#Inequality Process with Distributed Omega (IPDO)
set.seed(1)
IP = function (N=1000, time=500, omegas=c(0), pFreq=c(N), 
               record=c(100, 200, 300, 400, 500), longForm=TRUE){
  library(dplyr)
  library(tidyr)
  set.seed(1)
  id <- 1:N
  df <- data.frame(id=id)

  #Adjust to ensure that total pop. is N, and an even number
  if(N %% 2 != 0){
    print("N must be set to an even number!\nSetting N to N+1")
    N <- N+1}
  if(sum(pFreq) < N){
    nd <- N-sum(pFreq)
    ndg <- sample(1:6, 1)
    pFreq[ndg] <- pFreq[ndg] + nd
  }
  if(sum(pFreq) > N){
    nd <- sum(pFreq) - N
    ndg <- sample(1:6, 1)
    pFreq[ndg] <- pFreq[ndg] - nd
  }
  
  #Adding omegas column, group factor column
  oms <- rep(omegas, pFreq) #complete list
  df$omega <- oms
  ol <- length(omegas)
  groups <- as.numeric()
  for(i in 1:ol){
    groups[i] <- paste("group", i, sep="")
  }
  group <- rep(groups, pFreq)
  df$group <- group
  wealth <- rep(1, N)   #THIS IS A COUNTER, THAT IS CONTINUOUSLY REPLACED/UPDATED.  wealth = wealth at t-1.

  #Add additional columns for recording wealths at time t
  w <- data.frame(x=rep(NA,N))
  cs <- length(record)
  w <- w[,rep.int(1,cs)]
  names(w) <- record
  df <- dplyr::bind_cols(df, w)
  
  #Create workspace matrix
  work <- data.frame(wins=rep(NA, N/2), idi = rep(NA, N/2), idj = rep(NA, N/2), 
                     omega_i = rep(NA, N/2), omega_j = rep(NA, N/2), 
                     old_i = rep(NA, N/2), old_j = rep(NA, N/2),
                     newi=rep(NA, N/2), newj=rep(NA, N/2))
  #initialize wealths at 1.  This will be changed, but not necessarily recorded. 
    # Most recent wealths recorded here.  
  m <- 1
  indexes <- as.numeric()
  ## REPEAT T times..
  for(t in 1:time) {

    work$idi <- sample(x=id, size=N/2)
    work$idj <- sample(x=(id [! id %in% work$idi]), size=N/2)

    #Retrieve previous wealth from wealth vector(separate from df)
    work$old_i <- wealth[work$idi]    
    work$old_j <- wealth[work$idj]
    
    work$wins <- sample(0:1, N/2, replace=T)   ##Bernoulli
    
    #set omegas
    work$omega_i <- df$omega[work$idi]
    work$omega_j <- df$omega[work$idj]
    
    # transfer from loser to winner, omega(loser)*wealth(loser)
    work$newi <- work$old_i + work$wins*work$omega_j*work$old_j - (1-work$wins)*work$omega_i*work$old_i
    work$newj <- work$old_j - work$wins*work$omega_j*work$old_j + (1-work$wins)*work$omega_i*work$old_i
    
    #Send back to original wealths matrix
    
    if(t %in% record){
     # print(paste("RECORDING. t=", t, sep=""))
      vName <- t
      index <- grep(vName, colnames(df))
      indexes[m] <- index
      m <- m+1
      
    df[work$idi, index] <- work$newi
    df[work$idj, index]  <- work$newj
    }
    
    #print(work)
    
    #update old wealths
    wealth[work$idi] <- work$newi
    wealth[work$idj] <- work$newj
  }
  #Place in long-form
  if(longForm==TRUE){
  df <- tidyr::gather(df, step, wealth, indexes )}
  return (df) 
}
