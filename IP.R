#Inequality Process: 
#One Parameter Inequality Process (OPIP), and 
#Inequality Process with Distributed Omega (IPDO)
#set.seed(1)
IP = function (N=10000,  omegas=c(.05), pFreq=rep(N/length(omegas), length(omegas)), 
               record=c(500), time=max(record), longForm=TRUE, condense=TRUE, setSeed=FALSE, 
               wealthVarName=NULL, groupNames=NULL){
  library(lazyeval)
  library(dplyr)
  library(tidyr)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  if(setSeed==TRUE){set.seed(1)}
  id <- 1:N
  df <- data.frame(id=id)
  if (length(omegas)<length(pFreq)){
    omegas <- rep(omegas[1], length(pFreq))
  }
  cL <- length(omegas)
  pFreq <- round(pFreq/(sum(pFreq)/N)) ##this ensures the frequencies for each class sum to N
  #Adjust to ensure that total pop. is N, and an even number
  
  if(N %% 2 != 0){
    message("N must be set to an even number!\nSetting N to N+1")
    N <- N+1}
  while(sum(pFreq) < N) {
    #nd <- N-sum(pFreq)
    ndg <- sample(1:cL, 1) #setting index
    pFreq[ndg] <- pFreq[ndg] + 1
  }
  while(sum(pFreq) > N) {
    #nd <- sum(pFreq) - N
    ndg <- sample(1:cL, 1) #setting index
    pFreq[ndg] <- pFreq[ndg] - 1
  }
  
  #Adding omegas column, group factor column
  oms <- rep(omegas, pFreq) #complete list
  df$omega <- oms
  ol <- length(omegas)
  groups <- as.numeric()
  gr <- groupNames[1] %||% "group"
  if(length(groupNames)==ol){
    group <- rep(groupNames, pFreq)
  } else {
    for(i in 1:ol){
      groups[i] <- paste(gr, i, sep="")
    }
    group <- rep(groups, pFreq)
  }

  df$group <- group
  wealth <- rep(1, N)   #THIS IS A COUNTER, THAT IS CONTINUOUSLY REPLACED/UPDATED.  wealth = wealth at t-1.

  #Add additional columns for recording wealths at time t
  cs <- length(record)
  w <- data.frame(x=rep(NA,N))
  if(cs>1){w <- w[,rep.int(1,cs)]}
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

    #update old wealths
    wealth[work$idi] <- work$newi
    wealth[work$idj] <- work$newj
  }
  #Place in long-form
  if(longForm==TRUE){
    if(!is.null(wealthVarName)) {
      df <- tidyr::gather_(df, "step", wealthVarName, as.character(record))
  } else {
    df <- tidyr::gather(df, step, wealth, indexes )
  }

  if(condense==TRUE){ ##condense only makes sense if longForm is set to TRUE
    if(!is.null(wealthVarName)) {
      wealthV <- as.name(wealthVarName)
      dots <- list(~first(omega), ~first(group), ~mean(wealthV, na.rm=T))
      df <- df %>% group_by(id) %>%  
        summarise_(.dots = setNames(dots, c("omega", "group", wealthVarName)))
      
    } else {
      df <- df %>% group_by(id) %>% 
        summarise(omega=first(omega), group=first(group),
                  wealth=mean(wealth, na.rm=T))
    }
  }
  }
  return (df) 
}
