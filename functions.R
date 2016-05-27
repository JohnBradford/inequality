
`%+%` <- function(a, b) paste0(a, b)
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%#%` <- function(a, b) if (exists(a)) a else b
#function_that_might_return_null() %||% default value

library(dplyr)
library(tidyr)
library(GenSA)
source(file="IP.R")
source(file="cpsLoad.R")
source("dataFit.R")
source("sim_anneal.R")
source("anneal.R")

##Possible X's (conditioning variables):
##x=c("educ6", "educ4", "sex", "race", "race2", "hispan"),
##y=c("wage5", "wage10"),
cpsData <- loadCPS(x="educ6", y="wage5", years=1964:2015, pop=10000)
#produces list.  1. education df.  2.  education & wage df.
cps.x <- cpsData[[1]]
cps.x.y <- cpsData[[2]]
rm(cpsData)

##TESTS
#1.  TEST ip
x <- IP(N=10000,  omegas=rep(0.5, 6), record=c(500),
        longForm=TRUE, condense=TRUE, setSeed=FALSE, 
        wealthVarName=NULL, groupNames=NULL)

#2.  Test fit.data
##values = omegas = % lost by loser.  Higher omega = lower education class.   
# parameters:  (values, df.y, y.varNames=c("xgroup", "ygroup", "fit"),
 #                     df.x, x.varNames=c("xgroup", "Freq", "medW"), 
 #                     yr=NULL, setSeed=FALSE)

values=rep(0.5, 6)
fit.data(values, df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
                     df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
                    yr=1964, setSeed=FALSE)
best1986 <- c(.4733, .4261, .3674, .3162, .2528, .1940)
values <- best1986

best1986 <- c(.4733, .4261, .3674, .3162, .2528, .1940)  #overall = .0037
v0 <- c(.5, .45, .40, .35, .30, .25) #overall = .0032
bestEx1 <- c(0.5, 0.45, 0.36, 0.32, 0.28, 0.25) #experimental results
bestEx2 <- c(0.50, 0.45, 0.38, 0.35, 0.29, 0.25)
bestEx3 <- c(0.50, 0.45, 0.36, 0.35, 0.28, 0.25)
ExpWts1 <- c(0.500, 0.440, 0.348, 0.310, 0.258, 0.240)  #0.001760348
LocalEx <- c(0.6, 0.53, 0.443, 0.371, 0.354, 0.35)
LocalExWts <- c(0.5, 0.44, 0.33, 0.31, 0.258, 0.24)


pars <- list(best1986, bestEx1, bestEx2, bestEx3, ExpWts1, LocalEx, LocalExWts)
args <- list(df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
              df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
              yr=1986, setSeed=FALSE)
bestPars <- list()
parsdf <- data.frame(index=1:100, Angle1986=NA, Ex1=NA, Ex2=NA, Ex3=NA, ExpWts=NA, LocalEx=NA, LocalExWts=NA)
for(t in 1:100){
for(i in 1:length(pars)){
r <- as.numeric(do.call(fit.data, args=c(list(values=pars[[i]]), args))[[1]][1])
parsdf[t, i+1] <- r
}}
summary(parsdf)

do.call(fit.data, args=c(list(values=bestEx4), args))
#######################################################
##TEST WITH fitData.sweep algorithm
resultsWts <- fitData.sweep(wtsVar=cps.x$wts[which(cps.x$year==1986)], fits=3)

results <- fitData.sweep(fits=3)



output <- results[[1]]
results[2]  ##0.009147837
results[3]
results[4]
results[5]

outputWts <- resultsWts[[1]]
resultsWts[2]   ##0.001760348
resultsWts[3]   ## 0.500 0.440 0.348 0.310 0.258 0.240 , EXPWTS1
resultsWts[4]
resultsWts[5]



fit.data(values=resultsWts$Global_par, df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
         df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
         yr=1986, setSeed=FALSE)

fit.data(values=resultsWts$Localpar, df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
         df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
         yr=1986, setSeed=FALSE)


fit.data(values=results$Global_par, df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
         df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
         yr=1986, setSeed=FALSE)

fit.data(values=results$Localpar, df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
         df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), 
         yr=1986, setSeed=FALSE)

##3 Simulated Annealing algorithm, fitting par values - omegas (% lost by losers for each education category)
## TESTING ONE YEAR ONLY
numCalls <<- 0
test <- GenSA(par=rep(0.5, 6), fn=fit.data, 
              lower=rep(.001, 6), upper=rep(.999,6), 
              control=list(temperature=.5, 
                           threshold.stop=.005,
                           max.call=100, 
                           trace.mat=TRUE,
                           #maxit=20,
                           verbose=TRUE), 
              df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
              df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=1986)


niter <- 100
numCalls <<- 0
gr_optim_fun <- function(){
  pars<-rep(0.5, 6)
  optim_permanentTemp <<- rep(c(1,rep(0.5, niter-1)), niter)
  Lp <- length(pars)
  plus_minus <- sample(c(-1,1), Lp, replace=T)
  rvec <- runif(Lp, min=0, max=pars)*plus_minus*optim_permanentTemp[numCalls]
  return(pars+rvec)
}

test2 <- optim(par=rep(0.5, 6), fn=fit.data, 
               df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
               df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=1986,
               method="SANN", 
               control=list(trace=10, ndeps=.1, maxit=10,
                            abstol=.005, reltol=.001, REPORT=100,
                            temp=.9, tmax=10))


##3.  SIMULATED ANNEALING ALGORITHM
test3 <- simulated_annealing(s0=rep(0.5, 6), func=fit.data, 
                           args=list(df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
                                     df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=1986),
                           niter = 100, step = 0.001)

##for SANN method, temperature = temp / log(((t-1) %/% tmax)*tmax + exp(1))
temp <- 1; tmax <- 1
for(t in 1:10) {
  T <- temp / log(((t-1) %/% tmax)*tmax+exp(1))
  print(T)
}


#######################################################################################
output <- data.frame(yr=2010:2015, SS=NA, iters=NA)
output$par <- list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
for(yr in 2010:2015){
  numCalls <<- 0
  result <- GenSA(par=rep(0.5, 6), fn=fit.data, 
                  lower=rep(.001, 6), upper=rep(.999,6), 
                  control=list(temperature=c(1, 0.5, 0.5, 0.5, 0.5), 
                               threshold.stop=.005,
                               max.call=100, 
                               trace.mat=TRUE,
                               #maxit=20,
                               verbose=TRUE), 
                  df.y=cps.x.y, y.varNames=c("x", "y", "fit"),
                  df.x=cps.x, x.varNames=c("x", "pFreq.x", "medW.x"), yr=yr)
  
  output$SS[which(output$yr==yr)] <- result$value
  output$par[which(output$yr==yr)] <- list(result$par)
  output$iters[which(output$yr==yr)] <- numCalls

    
}









#lifetime earnings, looking at those who are 25+, whose income is > 0, and also counting years of data available and average age
nlAv <- subset(nl, age>=25 & wage09>0 & wage09<=100001)
nlAv <-  nlAv %>% group_by(ID) %>% 
  summarise(n=n(), #how many times person i appears
            asvab=first(ASVAB),
            race=first(RACE),
            sex=first(SEX),
            mEduc=mean(educ, na.rm=T),
            mEduc2=sum(educ, na.rm=T)/n,  #should be the same as first measure
            maxEduc=max(educ, na.rm=T),
            mwage09=mean(wage09, na.rm=T),
            mwage09_2=sum(wage09, na.rm=T)/n, #should be same
            medWage=median(wage09, na.rm=T),
            minwage09=min(wage09, na.rm=T),
            varwage09=var(wage09, na.rm=T),
            maxwage09=max(wage09, na.rm=T),
            totalWage09 = sum(wage09, na.rm=T),
            mAge = mean(age, na.rm=T),
            minage = min(age,na.rm=T),
            mwage_deMean=mean(deMeanedWage, na.rm=T),
            mwage_deMean_2=sum(deMeanedWage, na.rm=T)/n, #should be same
            medWage_deMean=median(deMeanedWage, na.rm=T),
            minwage_deMean=min(deMeanedWage, na.rm=T),
            varwage_deMean=var(deMeanedWage, na.rm=T),
            maxwage_deMean=max(deMeanedWage, na.rm=T),
            totalWage_deMean=sum(deMeanedWage, na.rm=T),
            mchng=mean(dWealth, na.rm=T),
            mchng_2=sum(dWealth, na.rm=T)/n, #should be same
            medchng=median(dWealth, na.rm=T),
            minchng=min(dWealth, na.rm=T),
            varchng=var(dWealth, na.rm=T),
            maxchng=max(dWealth, na.rm=T),
            mRank=mean(year.rank, na.rm=T),
            medRank=median(year.rank, na.rm=T),
            varRank=var(year.rank, na.rm=T),
            minRank=min(year.rank, na.rm=T),
            maxRank=max(year.rank, na.rm=T))
row.names(nlAv) <- 1:nrow(nlAv)  

