
`%+%` <- function(a, b) paste0(a, b)
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%#%` <- function(a, b) if (exists(a)) a else b
#function_that_might_return_null() %||% default value

library(dplyr)
library(tidyr)
library(GenSA)
#library(lazyeval)

source(file="IP.R")

#READING DATA
cps <- read.csv("cps.csv")
cps <- subset(cps, wage09 > 0 & wage09 < 100001)
cps$EDUC <- factor(cps$EDUC,levels(cps$EDUC)[c(2,6,3,5,1,4)])
cps$educ4 <- factor(cps$educ4,levels(cps$educ4)[c(3,2,4,1)])
nl <- read.csv("nlsy.csv")
nl$educ4 <- factor(nl$educ4,levels(nl$educ4)[c(3,2,4,1)])

##smaller dataframe, everything necessary to run the algorithm below
#only need the following:  
#cps.yr.ed$pFreq.educ, cps.yr.ed$medW.educ, cps.yr.ed$prcnt.edu (weights), and cps.yr.ed.wage$fit
pop <- 10000 
cps.yr.ed <- cps %>% group_by(YEAR, EDUC) %>%
  summarise(medW.educ=median(wage09, na.rm=T)) %>% 
  mutate(n.educ=n(),
    prcnt.educ=n.educ/sum(n.educ),
         pFreq.educ=ceiling(pop*prcnt.educ)) %>% 
  complete(EDUC) #from tidyr package, retain bins with zero cases
cps.yr.ed$wts <- cps.yr.ed$prcnt.educ

cps.yr.ed.wage<- cps %>% group_by(YEAR, EDUC, wage.cut5) %>%
  summarise(n=n()) %>% 
  mutate(prcnt=n/sum(n)) 
cps.yr.ed.wage$fit <- cps.yr.ed.wage$prcnt


yrs <- sort(unique(cps.yr.ed$YEAR))
inc <- sort(unique(cps.yr.ed.wage$wage.cut5))
educ <- sort(unique(cps.yr.ed$EDUC))


##frequencies for cps data, education, wage.cut5
#set pop to however many particles/agents you want to use in simulation

# cps.year <- cps %>% group_by(YEAR) %>%
#   summarise(n.yr=n(), 
#             mW.yr=mean(wage09, na.rm=T),
#             medW.yr=median(wage09, na.rm=T))

# cps.yr.wage <- cps %>% group_by(YEAR, wage.cut5) %>%
#   summarise(n.wage=n(), mW.wage=mean(wage09, na.rm=T),
#             medW.wage=median(wage09, na.rm=T)) %>% 
#   mutate(prcnt.wage=n.wage/sum(n.wage),
#          pFreq.wage=ceiling(pop*prcnt.wage)) %>% 
#   complete(wage.cut5) #from tidyr package, retain bins with zero cases
# 
# cpsF <- dplyr::full_join(cps.yr.ed.wage, cps.yr.ed, by=c("YEAR", "EDUC"))
# cpsF <- dplyr::full_join(cpsF, cps.yr.wage, by=c("YEAR", "wage.cut5"))
# cpsF <- dplyr::full_join(cpsF, cps.year, by=c("YEAR"))



#pFreq for simulations equal the # of members in each education level, summing to pop (1,000)
values=c(0.5)
source("dataFit.R")
output <- data.frame(yr=2010:2015, SS=NA, iters=NA, numErrors=NA)
output$par <- list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
for(yr in 2010:2015){
  numCalls <<- 0
  #numErrors <<-0
  result <- GenSA(par=rep(0.5, 6), fn=fit.data, 
        lower=rep(.001, 6), upper=rep(.999,6), 
        control=list(temperature=1, 
                     threshold.stop=.005,
                     max.call=100,
                     verbose=TRUE), 
        fit.df=cps.yr.ed.wage, educ.df=cps.yr.ed, yr=yr)
  
  output$SS[which(output$yr==yr)] <- result$value
  output$par[which(output$yr==yr)] <- list(result$par)
  output$iters[which(output$yr==yr)] <- numCalls
  #output$numErrors[which(output$yr==yr)] <- numErrors
    
}
best <- c(.4733, .4261, .3674, .3162, .2528, .1940)




test <- GenSA(par=rep(0.5, 6), fn=fit.data, 
                  lower=rep(.001, 6), upper=rep(.999,6), 
                  control=list(temperature=1, 
                               threshold.stop=.005,
                               max.call=100,
                               verbose=TRUE), 
              fit.df=cps.yr.ed.wage, educ.df=cps.yr.ed, yr=1986)




fit.data(values=c(0.5), fit.df=cps.yr.ed.wage, educ.df=cps.yr.ed, yr=1964)

x <- IP(N=10000, time=400, omegas=values, pFreq=pFreq, 
        record=c(400), longForm=TRUE, condense=TRUE)

x2 <- IP(N=1000, time=400, omegas=values, pFreq=pFreq, 
        record=c(400), longForm=TRUE, condense=TRUE)


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



#####OLD DATA ORDERING FUNCTIONS########
#note the 'wealth' or income variable has to be named 'wage09'
# groupData <- function(df, var1, var2=var1, var3=var1, pop=1000) {
#   df <- df %>% group_by_(var1, var2, var3) %>% 
#     summarise_(n=~n(), 
#                 meanW=~mean(wage09, na.rm=T),
#                 medW=~median(wage09, na.rm=T)) %>%   
#    mutate(prcnt=(n/sum(n)),
#           pFreq=round(pop*prcnt)) %>% 
#     complete_(var3) #from tidyr package, retain bins with zero cases
#   return(df)
# }
# 
# df2 <- groupData(cps, var1="YEAR", var2="EDUC", var3="wage.cut5")
# df2 <- df2[which(df2$YEAR==1986),]
# 
# groupDataY2 <- function(df, var1, var2=var1, wealthVarNum=NULL, pop=1000, yearVarNum=NULL, y=NULL) {
#   if(!is.null(yearVarNum) & !is.null(y)) {
#     df <- df[which(df[,yearVarNum]==y),] #subset year
#   }
#   df <- group_by_(df, var1, var2) %>% 
#     mutate(n=n(), prcnt=(n/sum(n)),
#                      pFreq=round(pop*prcnt))
#   wVar <- as.name(names(df)[wealthVarNum])
#    df <- summarise_each_(df, funs(mean, median), vars=c(wVar, "n","prcnt","pFreq")) %>% 
#      #ungroup() %>% 
#      mutate(n2=n(), prcnt2=(n2/sum(n_mean)),
#            pFreq2=round(pop*prcnt2)) %>%
#      complete_(var2)
#   return(df)
# }
# df <- groupDataY2(cps, var1="EDUC", var2="wage.cut5", wealthVarNum=11, yearVarNum=2, y=1986)
# 
# groupDataY <- function(df, var1, var2=var1, wealthVarNum=NULL, pop=1000, yearVarNum=NULL, y=NULL) {
#   if(!is.null(yearVarNum) & !is.null(y)) {
#     df <- df[which(df[,yearVarNum]==y),] #subset year
#   }
#   
#   df1 <- group_by_(df, var1) %>% 
#     mutate(n1=n(), prcnt1=(n1/sum(n1)),
#            pFreq1=round(pop*prcnt1))
#   #df1$v1 <- df1
#   dff <- df1
#   
#   if(var1!=var2){
#     df2 <- group_by_(df, var2) %>% 
#       mutate(n2=n(), prcnt2=(n2/sum(n2)),
#              pFreq2=round(pop*prcnt2))
#     
#     df3 <- group_by_(df, var1, var2) %>% 
#       mutate(n3=n(), prcnt3=(n3/sum(n3)),
#              pFreq3=round(pop*prcnt3))
#     
#     df3 <- dplyr::full_join(df3, df1, by=var1)
#     df3 <- dplyr::full_join(df3, df2, by=var2)
#   
#     dff <- df3
#   }
#   return(dff)
# }
# df <- groupDataY(cps, var1="EDUC", var2="wage.cut5", wealthVarNum=11, yearVarNum=2, y=1986)
# 
# 

