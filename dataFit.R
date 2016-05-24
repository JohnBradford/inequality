library(dplyr)
library(tidyr)
#library(lazyeval)
source(file="IP.R")

`%+%` <- function(a, b) paste0(a, b)
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%#%` <- function(a, b) if (exists(a)) a else b
#function_that_might_return_null() %||% default value

##the 'fit.data' function needs the following items:

##Freq and medianIncome both have length equal to levels(x), e.g. found in cps.x df
##fit has length equal to levels(x)*levels(y), where levels(y) are the income bins
fit.data <- function(values, df.y, y.varNames=c("xgroup", "ygroup", "fit"),
                     df.x, x.varNames=c("xgroup", "Freq", "medW"), 
                     yr=NULL, setSeed=FALSE, multiple=TRUE) {

  if(!is.data.frame(df.y)){
    message("df.y MUST BE A DATAFRAME CONTINGENCY TABLE containing three variables in the following order: \n
            1. 'xgroup' = levels of categorical/ordinal x variable, e.g. Education.\n
            2. 'ygroup' = levels of categorical/ordinal x variable, e.g. Income Level.\n
            3. 'fit' = percentage of x in each y group.")
    stop()
  } else {
    if(!is.null(yr) & !is.null(df.y[,"year"])) {
      df.y <- subset(df.y, year==yr)}
    
    df.y <- df.y[complete.cases(df.y),]
    
    #creating three vars, if don't already exist
    if(is.null(df.y$xgroup)) {names(df.y)[which(names(df.y)==y.varNames[1])] <- "xgroup"}
    if(is.null(df.y$ygroup)) {names(df.y)[which(names(df.y)==y.varNames[2])] <- "ygroup"}
    if(is.null(df.y$fit)) {names(df.y)[which(names(df.y)==y.varNames[3])] <- "fit"}
  }
  
  if(sum(is.na(df.y$fit)>0)) {
    message("fitted vector cannot having missing values")
    stop()
  }

  if(!is.data.frame(df.x)){
      message("df.x MUST BE A DATAFRAME containing three variables in the following order:\n
        1. 'xgroup' = levels of categorical/ordinal x variable.\n
        2. 'Freq' = Frequencies in each x level. \n
        3. 'medW' = median income at each x level,
        4.  'wts' -OPTIONAL - percentage of total population belonging to x level")
      stop()
    } else {
      if(!is.null(yr) & !is.null(df.x[,"year"])) {
      df.x <- subset(df.x, year==yr)}
      
      df.x <- df.x[complete.cases(df.x),]
      
      #creating three vars, if don't already exist
      if(is.null(df.x$xgroup)) {names(df.x)[which(names(df.x)==x.varNames[1])] <- "xgroup"}
      if(is.null(df.x$Freq)) {names(df.x)[which(names(df.x)==x.varNames[2])] <- "Freq"}
      if(is.null(df.x$medW))  {names(df.x)[which(names(df.x)==x.varNames[3])] <- "medW"}
    }
  
  gNames <- unique(as.character(df.x$xgroup))
  Freq <- df.x$Freq
  if(setSeed==TRUE){set.seed(1)}
  
  dfs <- IP(N=10000,  omegas=values, pFreq=Freq,  record=c(500), longForm=TRUE,
          condense=TRUE, setSeed=FALSE, groupNames=gNames)
  
  ####fitting#####
  ##dfs = simulated data.
  dfs$xgroup <- dfs$group ##factors now unordered
  dfs$xgroup <- factor(dfs$xgroup, levels=gNames)  

  #Organize simulated data by x, e.g. education level - 6 or 4
  dfs.x <- dfs %>% group_by(xgroup) %>% 
    summarise(n=n(),
              meanW=mean(wealth, na.rm=T),
              medW=median(wealth, na.rm=T)) %>% 
    mutate(wts = n / sum(n, na.rm=T)) %>% 
    complete(xgroup)
  
  dfs.x$meanW[is.na(dfs.x$meanW)] <- 0
  dfs.x$medW[is.na(dfs.x$medW)] <- 0
  dfs.x$wts[is.na(dfs.x$wts)] <- 0
  
  #if empirical population weights already provided, replace estimated with actual wts
  if(!is.null(df.x$wts)) {dfs.x$wts <- df.x$wts}
  
  dfs.x$realMedian[which(dfs.x$xgroup==df.x$xgroup)] <- df.x$medW
  dfs.x$estMean <- (dfs.x$realMedian/dfs.x$medW)*dfs.x$meanW #conditional Mean estimate - education class
  Umean <- sum(dfs.x$wts*dfs.x$estMean, na.rm=T)  ##Unconditional Mean Estimate Equals $32052.13 for 1986
  dfs$estWealth <- dfs$wealth*Umean
  #xx <- x
  dfs <- subset(dfs, estWealth > 0 & estWealth < 100001) 
  #throwout incomes over $100k 
  ibreaks <- c(0, 20000, 40000, 60000, 80000, 100000) 
  if(length(levels(df.y$ygroup))==10){
    ibreaks <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
  }
  dfs$ygroup <- cut(dfs$estWealth, ibreaks, right=TRUE)
  #dfs$xgroup <- dfs$xgroup
  ##GROUP BY educATION LEVEL, INCOME LEVEL
  dfs.xy  <- dfs %>% group_by(xgroup, ygroup) %>% 
    summarise(n=n()) %>% 
    mutate(predict = n / sum(n, na.rm=T)) %>% 
    complete(ygroup)
  
  dfs.xy$predict[is.na(dfs.xy$predict)] <- 0
  
  dfJoined <- left_join(df.y, dfs.xy, by=c("xgroup", "ygroup"))
  dfJoined$wts <- 0
  for(i in seq_along(levels(dfJoined$xgroup))){
  dfJoined$wts[which(dfJoined$xgroup==levels(dfJoined$xgroup)[i])] <- dfs.x$wts[which(dfs.x$xgroup==levels(dfs.x$xgroup)[i])]
  
  
  }
  
  dfJoined$fit[is.na(dfJoined$fit)] <- 0
  dfJoined$predict[is.na(dfJoined$predict)] <- 0
  dfJoined$wts[is.na(dfJoined$wts)] <- 0
  
  Total_sq_res <- sum( ((dfJoined$fit-dfJoined$predict)^2)*dfJoined$wts, na.rm=T)
  
  sqrs <- as.numeric()
  if(multiple==TRUE){ #vector of squared residuals for each xgroup
    for(i in seq_along(levels(dfJoined$xgroup))){
      dfJ <- dfJoined[which(dfJoined$xgroup==levels(dfJoined$xgroup)[i]),]
      group_sqrs <- sum( ((dfJ$fit-dfJ$predict)^2)*dfJ$wts, na.rm=T)
      sqrs[i] <- group_sqrs
    }
    }

  if (exists("numCalls")) {
    numCalls <<- numCalls + 1
  }
  if(multiple==TRUE){return(list(Total_sq_res, sqrs))} else {return(Total_sq_res)}
}