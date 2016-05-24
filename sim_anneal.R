simulated_annealing <- function(s0, func, args, niter = 100, ncyc=8, reset=FALSE, ...) {
  Lp <- length(s0)
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  args_n <- c(list(values=s0), args)
  #message(args)
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- do.call(func, args_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  #Tempr <- rep(c(1,rep(0.5, niter-1)), niter)
  #8 temps per iteration, then reset.
for (k in 1:niter) {
    if(reset==TRUE){s_c <- s0}  #reset
    Tempr <- c(1,rep(0.5, ncyc-1))
  for(t in 1:length(Tempr)) {
    Temp <- Tempr[t]
    #Temp <- Tempr[t]^(t-1)
    #Temp <- (1 - step)^t
    # consider a random neighbor
    plus_minus <- sample(c(-1,1), Lp, replace=T)
    rvec <- runif(Lp, min=0, max=s_c)*plus_minus*Temp
    s_n <- s_c + rvec
    print(rvec)
    #s_n <- runif(Lp, min=0, max=1)
    #s_n <- rnorm(Lp, s_c, 1)
    args_n <- c(list(values=s_n), args)
    #message(args)
    f_n <- do.call(func, args_n)
    
    # update current state
    #if (f_n < f_c ||  runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) {
    #if (f_n < f_c || runif(Lp, 0, 1) < exp(-(f_n - f_c) / Temp)) {
    if (f_n < f_c) {
      s_c <- s_n
      #s_c <- (s_n + s_c)/2 ##or take average
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b))
}


#exp( (f_c - f_n) / Tempr)
# pars = 6 numbers, e.g. .1, .2, .3, .4, .5, .6
# 6 education classes:
