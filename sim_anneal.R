simulated_annealing <- function(s0, func, args, niter = 10, step = 0.001, ...) {
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
  Tempr <- c(1,rep(0.5, niter-1))
  for (k in 1:niter) {     
    Temp <- Tempr[k]
    #Temp <- (1 - step)^k
    # consider a random neighbor
    s_n <- runif(Lp, min=0, max=1)
    #s_n <- rnorm(Lp, s_c, 1)
    args_n <- c(list(values=s_n), args)
    #message(args)
    f_n <- do.call(func, args_n)
    
    # update current state
    if (f_n < f_c || runif(Lp, 0, 1) < exp(-(f_n - f_c) / Temp)) {
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b))
}

sol <- simulated_annealing(s0=rep(0.5, 6), func=fit.data, 
          args=list(df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
          df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=1986),
          niter = 10, step = 0.001)