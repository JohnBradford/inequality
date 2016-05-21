
loadCPS <- function(x=c("educ6", "educ4", "sex", "race", "race2", "hispan"),
                    y=c("wage5", "wage10"), years=1964:2015, pop=10000){
 
  `%+%` <- function(a, b) paste0(a, b)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  `%#%` <- function(a, b) if (exists(a)) a else b
  #function_that_might_return_null() %||% default value
  library(dplyr)
  library(tidyr)
  
#READING DATA-CPS
olddir <- getwd()
setwd("dataFiles")
cps <- read.csv("cps.csv")
names(cps) <- c("none", "year", "wts", "age", "sex", "race", "hispan", "educ6", "race2", "educ4",
                "wage", "wage5", "wage10", "uMean09", "deMeanedWage")
setwd(olddir)  

##Possible X's (conditioning variables):
##EDUC, educ4, SEX, RACE, RACE2, HISPAN, AGE (needs to be cut)
# [1] "X"            "YEAR"         "WTSUPP"       "AGE"          "SEX"          "RACE"        
# [7] "HISPAN"       "EDUC"         "RACE2"        "educ4"        "wage09"       "wage.cut5"   
# [13] "wage.cut10"   "uMean09"      "deMeanedWage"

cps <- cps[,c("year", "wage", x, y)]
cps <- subset(cps, wage > 0 & wage < 100001)

if(x=="educ6"){
  cps$educ6 <- factor(cps$educ6,levels(cps$educ6)[c(2,6,3,5,1,4)])
}
if(x=="educ4"){
  cps$educ4 <- factor(cps$educ4,levels(cps$educ4)[c(3,2,4,1)])
}
names(cps) <- c("year", "wage", "x", "y")  

#only need the following files to run dataFit algorithm:  
#cps.x$pFreq.x, cps.x$medW.x, cps.x$prcnt.x (weights), and cps.x.y$fit
cps.x <- cps %>% group_by(year, x) %>%
  summarise(n=n(), medW.x=median(wage, na.rm=T)) %>% 
  mutate(prcnt.x=(n/sum(n, na.rm=T)),
         pFreq.x=ceiling(pop*prcnt.x)) %>% 
  complete(x) #from tidyr package, retain bins with zero cases
cps.x$wts <- cps.x$prcnt.x

cps.x.y <- cps %>% group_by(year, x, y) %>%
  summarise(n=n()) %>% 
  mutate(prcnt=(n/sum(n, na.rm=T))) 
cps.x.y$fit <- cps.x.y$prcnt

cpsData <- list(cps.x, cps.x.y)
rm(cps)
return(cpsData)
}
