library(dplyr)
library(tidyr)
#library(lazyeval)
source(file="IP.R")

`%+%` <- function(a, b) paste0(a, b)
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%#%` <- function(a, b) if (exists(a)) a else b
#function_that_might_return_null() %||% default value


#e <<- new.env() #ifyou wanted to record results from other function call
fit.data <- function(values, fit.df, educ.df, yr=NULL, setSeed=FALSE) {
  df <- fit.df
  if(!is.null(yr)){ df <- subset(df, YEAR==yr)}
  df <- df[complete.cases(df),]
  dfEd <- educ.df
  if(!is.null(yr)){  dfEd <- subset(educ.df, YEAR==yr)}
  dfEd <- dfEd[complete.cases(dfEd),]
  gNames <- unique(dfEd$EDUC)
  pFreq <- dfEd$pFreq.educ
  if(setSeed==TRUE){set.seed(1)}
  
  x <- IP(N=10000,  omegas=values, pFreq=pFreq,  record=c(500), longForm=TRUE,
          condense=TRUE, setSeed=FALSE, groupNames=gNames)
  
  ####fitting#####
  x$group <- factor(x$group)
  nL <- length(levels(x$group))
  for(i in 1:nL) {
    levels(x$group)[i] <- levels(df$EDUC)[i]    ##matching education level to group factor
  }
  
  #Organize simulated data by education level - 6 or 4
  xEd <- x %>% group_by(group) %>% 
    summarise(n=n(),
              meanW=mean(wealth),
              medW=median(wealth)) %>% 
    mutate(wts = n / sum(n)) %>% 
    complete(group)
  
  # n <- length(levels(x$group))
  # for(i in 1:nL) {
  #   levels(x$group)[i] <- levels(df$EDUC)[i]    ##matching education level to group factor
  # }
  
  xEd$realMedian[which(xEd$group==dfEd$EDUC)] <- dfEd$medW.educ
  xEd$estMean <- (xEd$realMedian/xEd$medW)*xEd$meanW #conditional Mean estimate - education class
  Umean <- sum(xEd$wts*xEd$estMean)  ##Unconditional Mean Estimate Equals $32052.13 for 1986
  x$estWealth <- x$wealth*Umean
  #xx <- x
  x <- subset(x, estWealth > 0 & estWealth < 100001) 
  #throwout incomes over $100k 
  ibreaks5 <- c(0, 20000, 40000, 60000, 80000, 100000)
  ibreaks10 <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
  x$wage.cut5 <- cut(x$estWealth, ibreaks5, right=TRUE)
  x$wage.cut10 <- cut(x$estWealth, ibreaks10, right=TRUE)
  x$EDUC <- x$group
  ##GROUP BY EDUCATION LEVEL, INCOME LEVEL
  xPred  <- x %>% group_by(EDUC, wage.cut5) %>% 
    summarise(n=n()) %>% 
    mutate(predict = n / sum(n)) 
  
  dfJoined <- left_join(df, xPred, by=c("EDUC", "wage.cut5"))
  
  SSw <- sum(((dfJoined$fit-dfJoined$predict)^2)*dfJoined$wts)
  if (exists("numCalls")) {
    numCalls <<- numCalls + 1
  } else {message("numCalls doesn't exist here")}
  return(SSw)
}