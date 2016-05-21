##This file generates the data file, "nlsy.csv" used in analyses
olddir <- getwd()
setwd("dataFiles")
library(tidyr)
library(dplyr)

new_data <- read.table('Income_Educ.dat', sep=' ')
names(new_data) <- c('R0000100',
  'R0214700',
  'R0214800',
  'R0216500',
  'R0216701',
  'R0312300',
  'R0406401',
  'R0406510',
  'R0482600',
  'R0618301',
  'R0618901',
  'R0619010',
  'R0782101',
  'R0898201',
  'R0898310',
  'R1024001',
  'R1145001',
  'R1145110',
  'R1410701',
  'R1520201',
  'R1520310',
  'R1778501',
  'R1890901',
  'R1891010',
  'R2141601',
  'R2258001',
  'R2258110',
  'R2350301',
  'R2445401',
  'R2445510',
  'R2722501',
  'R2871101',
  'R2871300',
  'R2971401',
  'R3074801',
  'R3075000',
  'R3279401',
  'R3401501',
  'R3401700',
  'R3559001',
  'R3656901',
  'R3657100',
  'R3897101',
  'R4007401',
  'R4007600',
  'R4295101',
  'R4418501',
  'R4418700',
  'R4982801',
  'R5081700',
  'R5103900',
  'R5166901',
  'R5167000',
  'R5626201',
  'R6364601',
  'R6479600',
  'R6479800',
  'R6909701',
  'R7007300',
  'R7007500',
  'R7704600',
  'R7704800',
  'R8316300',
  'R8497000',
  'R8497200',
  'T0912400',
  'T0988800',
  'T0989000',
  'T2076700',
  'T2210700',
  'T2210800',
  'T3045300',
  'T3108600',
  'T3108700',
  'T3977400',
  'T4113000',
  'T4113200')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA
  new_data$R0214700 <- factor(new_data$R0214700, 
    levels=c(1.0,2.0,3.0), 
    labels=c("HISPANIC",
      "BLACK",
      "NON-BLACK, NON-HISPANIC"))
  new_data$R0214800 <- factor(new_data$R0214800, 
    levels=c(1.0,2.0), 
    labels=c("MALE",
      "FEMALE"))

  # 
  #  data$R0216701 <- factor(data$R0216701, 
  #   levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0), 
  #   labels=c("NONE",

  #     "UNGRADED"))
  # 
 

varlabels <- c("ID# (1-12686) 79",
  "RACL/ETHNIC COHORT /SCRNR 79",
  "SEX OF R 79",
  "AGE OF R @ INT DATE 79",
  "HIGHEST GRADE COMPLTD (REV) 79",
  "TOT INC WAGES AND SALRY P-C YR 80",
  "HIGHEST GRADE COMPLTD (REV) 80",
  "AGE OF R @ INT DATE 80",
  "TOT INC WAGES AND SALRY P-C YR 81",
  "PROFILES AFQT PRCTILE 2006 (REV) 81",
  "HIGHEST GRADE COMPLTD (REV) 81",
  "AGE OF R @ INT DATE 81",
  "TOT INC WAGES AND SALRY P-C YR 82 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 82",
  "AGE OF R @ INT DATE 82",
  "TOT INC WAGES AND SALRY P-C YR 83 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 83",
  "AGE OF R @ INT DATE 83",
  "TOT INC WAGES AND SALRY P-C YR 84 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 84",
  "AGE OF R @ INT DATE 84",
  "TOT INC WAGES AND SALRY P-C YR 85 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 85",
  "AGE OF R @ INT DATE 85",
  "TOT INC WAGES AND SALRY P-C YR 86 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 86",
  "AGE OF R @ INT DATE 86",
  "TOT INC WAGES AND SALRY P-C YR 87 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 87",
  "AGE OF R @ INT DATE 87",
  "TOT INC WAGES AND SALRY P-C YR 88 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 88",
  "AGE OF R @ INT DATE 88",
  "TOT INC WAGES AND SALRY P-C YR 89 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 89",
  "AGE OF R @ INT DATE 89",
  "TOT INC WAGES AND SALRY P-C YR 90 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 90",
  "AGE OF R @ INT DATE 90",
  "TOT INC WAGES AND SALRY P-C YR 91",
  "HIGHEST GRADE COMPLTD (REV) 91",
  "AGE OF R @ INT DATE 91",
  "TOT INC WAGES AND SALRY P-C YR 92 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 92",
  "AGE OF R @ INT DATE 92",
  "AMT OF R'S WAGES/SALARY/TIPS (PCY) 93 (TRUNC)",
  "HIGHEST GRADE COMPLTD (REV) 93",
  "AGE AT INTERVIEW DATE 93",
  "AMT OF R'S WAGES/SALARY/TIPS (PCY) 94",
  "AGE AT INTERVIEW DATE 94",
  "HIGHEST GRADE COMPLTD (REV) 94",
  "HIGHEST GRADE COMPLTD (REV) 96",
  "AGE AT INTERVIEW DATE 96",
  "AMT OF R'S WAGES/SALARY/TIPS (PCY) 96",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 1998",
  "HIGHEST GRADE COMPLTD (REV) 1998",
  "AGE AT INTERVIEW DATE 1998",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2000",
  "HIGHEST GRADE COMPLTD (REV) 2000",
  "AGE AT INTERVIEW DATE 2000",
  "HIGHEST GRADE COMPLTD (REV) 2002",
  "AGE AT INTERVIEW DATE 2002",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2004",
  "HIGHEST GRADE COMPLTD (REV) 2004",
  "AGE AT INTERVIEW DATE 2004",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2006",
  "HIGHEST GRADE COMPLTD (REV) 2006",
  "AGE AT INTERVIEW DATE 2006",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2008",
  "HIGHEST GRADE COMPLTD (REV) 2008",
  "AGE AT INTERVIEW DATE 2008",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2010",
  "HIGHEST GRADE COMPLTD (REV) 2010",
  "AGE AT INTERVIEW DATE 2010",
  "AMT OF RS WAGES/SALARY/TIPS (PCY) 2012",
  "HIGHEST GRADE COMPLTD 2012",
  "AGE AT INTERVIEW DATE 2012"
)


library(Hmisc)
c <- length(varlabels)
for(i in 1:c){
  label(new_data[[i]]) <- varlabels[i]
}


# Use qnames rather than rnums
  names(new_data) <- c("ID",
    "RACE",
    "SEX",
    "AGEATINT_1979",
    "HGCREV79_1979",
    "Q13-5_1980",
    "HGCREV80_1980",
    "AGEATINT_1980",
    "Q13-5_1981",
    "ASVAB",
    "HGCREV81_1981",
    "AGEATINT_1981",
    "Q13-5_TRUNC_REVISED_1982",
    "HGCREV82_1982",
    "AGEATINT_1982",
    "Q13-5_TRUNC_REVISED_1983",
    "HGCREV83_1983",
    "AGEATINT_1983",
    "Q13-5_TRUNC_REVISED_1984",
    "HGCREV84_1984",
    "AGEATINT_1984",
    "Q13-5_TRUNC_REVISED_1985",
    "HGCREV85_1985",
    "AGEATINT_1985",
    "Q13-5_TRUNC_REVISED_1986",
    "HGCREV86_1986",
    "AGEATINT_1986",
    "Q13-5_TRUNC_REVISED_1987",
    "HGCREV87_1987",
    "AGEATINT_1987",
    "Q13-5_TRUNC_REVISED_1988",
    "HGCREV88_1988",
    "AGEATINT_1988",
    "Q13-5_TRUNC_REVISED_1989",
    "HGCREV89_1989",
    "AGEATINT_1989",
    "Q13-5_TRUNC_REVISED_1990",
    "HGCREV90_1990",
    "AGEATINT_1990",
    "Q13-5_TRUNC_REVISED_1991",
    "HGCREV91_1991",
    "AGEATINT_1991",
    "Q13-5_TRUNC_REVISED_1992",
    "HGCREV92_1992",
    "AGEATINT_1992",
    "Q13-5_TRUNC_REVISED_1993",
    "HGCREV93_1993",
    "AGEATINT_1993",
    "Q13-5_TRUNC_REVISED_1994",
    "AGEATINT_1994",
    "HGCREV94_1994",
    "HGCREV96_1996",
    "AGEATINT_1996",
    "Q13-5_TRUNC_REVISED_1996",
    "Q13-5_TRUNC_REVISED_1998",
    "HGCREV98_1998",
    "AGEATINT_1998",
    "Q13-5_TRUNC_REVISED_2000",
    "HGCREV00_2000",
    "AGEATINT_2000",
    "HGCREV02_2002",
    "AGEATINT_2002",
    "Q13-5_TRUNC_2004",
    "HGCREV04_2004",
    "AGEATINT_2004",
    "Q13-5_TRUNC_2006",
    "HGCREV06_2006",
    "AGEATINT_2006",
    "Q13-5_TRUNC_2008",
    "HGCREV08_2008",
    "AGEATINT_2008",
    "Q13-5_TRUNC_2010",
    "HGCREV10_2010",
    "AGEATINT_2010",
    "Q13-5_TRUNC_2012",
    "HGC_2012",
    "AGEATINT_2012")



#********************************************************************************************************
  nlsy <- new_data
  
  nlEd <- nlsy[,c(1:3, 10, 5,7,11,14,17,20,23,26,29,32,35,38,41,44,47,51,52,56,59,61,64,67, 70,73,76)]
  nls1 <- nlsy[,-c(1:3,10, 5,7,11,14,17,20,23,26,29,32,35,38,41,44,47,51,52,56,59,61,64,67, 70,73,76)]
  nlAge <- nls1[,c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,32,35,37,38,40,42,44,46,48)]
  nlInc <- nls1[,-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,32,35,37,38,40,42,44,46,48)]
  nlAge$ID <- nlsy$ID
  nlInc$ID <- nlsy$ID
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  

  nlEdL <- gather(nlEd, year, educ, 5:29)
  nlEdL$year <- substrRight(nlEdL$year, 4)
  nlAge <- nlAge[,-15] #accidentally had a variable here I didn't need.
  nlAgeL <- gather(nlAge, year, age, 1:24)
  nlAgeL$year <- substrRight(nlAgeL$year, 4)
  nlIncL <- gather(nlInc, year, income, 1:23)
  nlIncL$year <- substrRight(nlIncL$year, 4)
  
  
  nl <- dplyr::left_join(nlEdL, nlAgeL, by=c("ID", "year"))
  nl <- dplyr::left_join(nl, nlIncL, by=c("ID", "year"))

  nl$educF <- factor(nl$educ,
    levels=c(0.0,93.0,94.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,95.0),
    labels=c("NONE",
      "PRE-KINDERGARTEN",
      "KINDERGARTEN",
      "1ST GRADE",
      "2ND GRADE",
      "3RD GRADE",
      "4TH GRADE",
      "5TH GRADE",
      "6TH GRADE",
      "7TH GRADE",
      "8TH GRADE",
      "9TH GRADE",
      "10TH GRADE",
      "11TH GRADE",
      "12TH GRADE",
      "1ST YR COL",
      "2ND YR COL",
      "3RD YR COL",
      "4TH YR COL",
      "5TH YR COL",
      "6TH YR COL",
      "7TH YR COL",
      "8TH YR COL OR MORE",
      "UNGRADED"))
  
  nl$educ4 <- as.character(nl$educF)
  nl$educ4[which(nl$educF=="1ST GRADE" | nl$educF=="2ND GRADE" | nl$educF=="3RD GRADE" |  nl$educF=="4TH GRADE" | nl$educF=="5TH GRADE" | nl$educF=="6TH GRADE" | nl$educF=="7TH GRADE" | nl$educF=="8TH GRADE"  | nl$educF=="9TH GRADE" | nl$educF=="10TH GRADE" |  nl$educF=="11TH GRADE")] <- "Less than High School"
  nl$educ4[which(nl$educF=="12TH GRADE")] <- "High School Graduate"
  nl$educ4[which(nl$educF=="1ST YR COL" | nl$educF=="2ND YR COL" | nl$educF=="3RD YR COL")] <-  "Some College"
  nl$educ4[which(nl$educF=="4TH YR COL" |  nl$educF=="5TH YR COL"  |  nl$educF=="6TH YR COL"  |  nl$educF=="7TH YR COL"  | nl$educF=="8TH YR COL OR MORE")] <- "4+ Years College"
  nl$educ4 <- factor(nl$educ4, levels=c("Less than High School", "High School Graduate", "Some College", "4+ Years College" ))
  
  #https://www.whitehouse.gov/sites/default/files/docs/cea_2015_erp_complete.pdf
  #Table B-3, Personal Consumption Expenditure (PCE) price index numbers, chain-type price indexes for GDP
  #Constant 2009 USD
  pce <- read.csv(file="PCE_chain.csv")
  pce$pce <- pce$PCE/100
  nl$wage09 <- nl$income
  label(nl[[11]]) <- "Wage Inc Const 2009 USD" 
  for(y in pce$YEAR){
    nl$wage09[which(nl$year == y)] <- nl$income[which(nl$year == y)] / (pce$PCE[which(pce$YEAR == y)]/100)
  }
  
  #Count only cases with positive income and less than $100k.
  n1 <- nrow(nl)
  nl <- subset(nl,  !is.na(wage09) & wage09 > 0 & wage09 <= 100001)
  n2 <- nrow(nl)  #retains about 52% of cases
  ibreaks5 <- c(0, 20000, 40000, 60000, 80000, 100000)
  ibreaks10 <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
  nl$wage.cut5 <- cut(nl$wage09, ibreaks5, right=TRUE)
  nl$wage.cut10 <- cut(nl$wage09, ibreaks10, right=TRUE)

  nl$asvab4 <- cut(nl$ASVAB, 4, labels=c("bottom quartile", "second quartile", "third quartile", "top quartile"))
 
  #Coding "Top Codes" - 
  # 1. 1979-1984 max = 75,001. 
  # 2. 1985-1988 max = 100,001
  # 3. 1989-1995 max = average of outlying values
  # 4. 1996 -    max = mean(top 2%)
  # nlE$wageIncome <- nlE$wage.cut
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(0,1e+04]"] <- "$1-$10k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(1e+04,2e+04]"] <- "$10-$20k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(2e+04,3e+04]"] <- "$20-$30k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(3e+04,4e+04]"] <- "$30-$40k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(4e+04,5e+04]"] <- "$40-$50k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(5e+04,6e+04]"] <- "$50-$60k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(6e+04,7e+04]"] <- "$60-$70k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(7e+04,8e+04]"] <- "$70-$80k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(8e+04,9e+04]"] <- "$80-$90k"
  # levels(nlE$wageIncome)[levels(nlE$wage.cut)=="(9e+04,1e+05]"] <- "$90-$100k"
  # 
  # 
 
  nlMeans <- nl %>% group_by(year) %>% 
    summarise(n=n(), yMean=mean(wage09, na.rm=T))
  
  nl$uMean09 <- NA
  for(i in nlMeans$year){
    nl$uMean09[which(nl$year==i)] <- nlMeans$yMean[which(nlMeans$year==i)]
  }
  nl$deMeanedWage <- nl$wage09/nl$uMean09
  
  #missing ages for year 1993
  id93 <- sort(intersect(unique(nl$ID[which(nl$year==1993)]), unique(nl$ID[which(nl$year==1992)])))
  for(i in id93){
    nl$age[which(nl$ID==i & nl$year==1993)] <- nl$age[which(nl$ID==i & nl$year==1992)] + 1
  }

  #RANKS min=highest wage, max=lowest wage
  row.names(nl) <- 1:nrow(nl)
  nl <- transform(nl,
            year.rank = ave(wage09, year,
    FUN = function(x) rank(-x, ties.method = "min")))
  
#FIND FORWARD DIFFERENCE, CHANGE IN INCOME FROM PREVIOUS YEAR
  #The lagged differences Wealth(t+1)-Wealth(t) are placed at time t
  #for easy regressions of past wealth on forward differences

  nl <- nl %>%
    group_by(ID) %>%
    mutate(dWealth = lead(deMeanedWage - lag(deMeanedWage)))
  setwd(olddir)           
  write.csv(nl, file="nlsy.csv")