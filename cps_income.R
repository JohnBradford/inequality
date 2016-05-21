##THIS FILE USED TO CREATE THE "cps.csv" data file.
olddir <- getwd()
setwd("dataFiles")
cps <- read.csv("cpsData.csv", na.strings=c(999, 9999999, 99999999, 9999998, 99999998, 99999, 99997, "", "NA"))
library(Hmisc)
library(dplyr)
library(tidyr)
label(cps[[1]]) <- "Survey Year"
label(cps[[3]]) <- "Household weight"
label(cps[[4]]) <- "Household Income"
label(cps[[7]]) <- "Individual weight"
label(cps[[13]]) <- "Total family income"
label(cps[[14]]) <-  "Total personal income"
label(cps[[15]]) <- "Wage and salary income"
label(cps[[16]]) <- "Income dividends, rent, trusts" #INCDRT
label(cps[[17]]) <- "Income from interest"  #INCINT
label(cps[[18]]) <- "Income from dividends" #INCDIVID
label(cps[[19]]) <- "Income from rent" #INCRENT

cps$nonWage <- cps$INCTOT - cps$INCWAGE
label(cps[[29]]) <- "Total Income Minus Wage Income"


cps$SEX <- as.factor(cps$SEX)
levels(cps$SEX) <- c("male", "female")
cps$RACE <- as.factor(cps$RACE)
#Treating PAcific Islanders as Amerindians, distinct from North/East Asians because only East/North Asians
#have higher average incomes and IQs.
levels(cps$RACE)[levels(cps$RACE)=="100"] <- "White"
levels(cps$RACE)[levels(cps$RACE)=="200"] <- "Black"
levels(cps$RACE)[levels(cps$RACE)=="300"] <- "Amerindian"
levels(cps$RACE)[levels(cps$RACE)=="650"] <- "Asian"  #Asian or Pacific Islander
levels(cps$RACE)[levels(cps$RACE)=="651"] <- "Asian"   #Asian only
levels(cps$RACE)[levels(cps$RACE)=="652"] <- "Amerindian"   #***!Hawaiian/Pacific Islander only See Note
levels(cps$RACE)[levels(cps$RACE)=="809"] <- "Asian"   #Asian-Hawaiian/Pacific Islander
levels(cps$RACE)[levels(cps$RACE)=="801"] <- "White-Black"
levels(cps$RACE)[levels(cps$RACE)=="802"] <- "White-Amerindian"
levels(cps$RACE)[levels(cps$RACE)=="803"] <- "White-Asian"
levels(cps$RACE)[levels(cps$RACE)=="804"] <- "White-Amerindian"  #See Note - White-Hawaiian/Pacific Islander
levels(cps$RACE)[levels(cps$RACE)=="805"] <- "Black-Amerindian"
levels(cps$RACE)[levels(cps$RACE)=="806"] <- "Black-Asian"
levels(cps$RACE)[levels(cps$RACE)=="807"] <- "Black-Amerindian"   #Black-Hawaiian/Pacific Islander See Note
levels(cps$RACE)[levels(cps$RACE)=="700" | levels(cps$RACE)=="808" | levels(cps$RACE)=="810" | levels(cps$RACE)=="811" | levels(cps$RACE)=="812" | levels(cps$RACE)=="813" | levels(cps$RACE)=="814" | levels(cps$RACE)=="815" | levels(cps$RACE)=="816" | levels(cps$RACE)=="817" | levels(cps$RACE)=="818" | levels(cps$RACE)=="819" | levels(cps$RACE)=="820" | levels(cps$RACE)=="830"] <- "Other" 

cps$RACE2 <- cps$RACE  #Coarser grained race categories, primacy to black, then white, Asian, then Amerindian
levels(cps$RACE2)[levels(cps$RACE2)=="White-Black"] <- "Black"
levels(cps$RACE2)[levels(cps$RACE2)=="Black-Amerindian"] <- "Black"
levels(cps$RACE2)[levels(cps$RACE2)=="Black-Asian"] <- "Black"
levels(cps$RACE2)[levels(cps$RACE2)=="White-Asian"] <- "White"
levels(cps$RACE2)[levels(cps$RACE2)=="White-Amerindian"] <- "White"
levels(cps$RACE2)[levels(cps$RACE2)=="White-Amerindian"] <- "White"

cps$HISPAN <- as.factor(cps$HISPAN)
levels(cps$HISPAN)[levels(cps$HISPAN)=="0"] <- "Non Hispanic"
levels(cps$HISPAN)[levels(cps$HISPAN)=="100" | levels(cps$HISPAN)=="102" | levels(cps$HISPAN)=="103" | levels(cps$HISPAN)=="104" | levels(cps$HISPAN)=="108" | levels(cps$HISPAN)=="109" | levels(cps$HISPAN)=="200" | levels(cps$HISPAN)=="300" | levels(cps$HISPAN)=="400" | levels(cps$HISPAN)=="500" | levels(cps$HISPAN)=="401" | levels(cps$HISPAN)=="410" | levels(cps$HISPAN)=="411" | levels(cps$HISPAN)=="412"] <- "Hispanic"
levels(cps$HISPAN)[levels(cps$HISPAN)=="901" | levels(cps$HISPAN)=="902"] <- NA

cps$EDUC <- as.factor(cps$EDUC)
levels(cps$EDUC)[levels(cps$EDUC)=="2" | levels(cps$EDUC)=="10" | levels(cps$EDUC)=="11" | levels(cps$EDUC)=="12"  | levels(cps$EDUC)=="13" | levels(cps$EDUC)=="14" | levels(cps$EDUC)=="20" | levels(cps$EDUC)=="21" | levels(cps$EDUC)=="22"  | levels(cps$EDUC)=="30" | levels(cps$EDUC)=="31" | levels(cps$EDUC)=="32"  ] <- "Eighth Grade or Less"
levels(cps$EDUC)[levels(cps$EDUC)=="40" | levels(cps$EDUC)=="50" | levels(cps$EDUC)=="60" | levels(cps$EDUC)=="70" | levels(cps$EDUC)=="71"] <- "Some High School"
#High School Graduate == Diploma OR "12th Grade Diploma Unclear"
levels(cps$EDUC)[levels(cps$EDUC)=="73" | levels(cps$EDUC)=="72"] <- "High School Graduate"
levels(cps$EDUC)[levels(cps$EDUC)=="80" | levels(cps$EDUC)=="81" | levels(cps$EDUC)=="90" | levels(cps$EDUC)=="91" | levels(cps$EDUC)=="92" | levels(cps$EDUC)=="100"] <- "Some College"
levels(cps$EDUC)[levels(cps$EDUC)=="111" | levels(cps$EDUC)=="110" ] <- "College Graduate"
#PostGraduate == 5+years, 6 years [Before 1992] OR masters, professional, doctorate [1992 and after]
levels(cps$EDUC)[levels(cps$EDUC)=="123" | levels(cps$EDUC)=="124" | levels(cps$EDUC)=="125" | levels(cps$EDUC)=="120" | levels(cps$EDUC)=="121" | levels(cps$EDUC)=="122"] <- "Post Graduate" 


cps$educ4 <- as.character(cps$EDUC)
cps$educ4[which(cps$EDUC=="Eighth Grade or Less" | cps$EDUC=="Some High School")] <-  "Less than High School"
cps$educ4[which(cps$EDUC=="College Graduate"  | cps$EDUC=="Post Graduate")] <-  "4+ Years College" 
cps$educ4 <- factor(cps$educ4, levels=c("Less than High School", "High School Graduate", "Some College", "4+ Years College" ))



#https://www.whitehouse.gov/sites/default/files/docs/cea_2015_erp_complete.pdf
#Table B-3, Personal Consumption Expenditure (PCE) price index numbers, chain-type price indexes for GDP
#Constant 2009 USD
pce <- read.csv(file="PCE_chain.csv")
pce$pce <- pce$PCE/100
cps$wage09 <- cps$INCWAGE
label(cps[[31]]) <- "Wage Inc Const 2009 USD" 
for(y in pce$YEAR){
  cps$wage09[which(cps$YEAR == y)] <- cps$INCWAGE[which(cps$YEAR == y)] / (pce$PCE[which(pce$YEAR == y)]/100)
}

#Count only cases with positive income and less than $100k.
n1 <- nrow(cps)
#cps <- subset(cps, wage09 > 0 & wage09 < 100001)
n2 <- nrow(cps)
# n1 - n2
# n2/n1
#Eliminates 1954308 cases, 62% of original with wage09>0, and with both constraints, 
## only ~ 60% of original
#Next, 1963 is missing data, get rid of it, begin with 1964
cps <- subset(cps, YEAR>1963)
n3 <- nrow(cps)

ibreaks5 <- c(0, 20000, 40000, 60000, 80000, 100000)
ibreaks10 <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
cps$wage.cut5 <- cut(cps$wage09, ibreaks5, right=TRUE)
cps$wage.cut10 <- cut(cps$wage09, ibreaks10, right=TRUE)
#Foreach Education category (or race or ...), percentage in each Income Bin


##Percentage of each Education Category distributed across income bins
# cpsE$wageIncome <- cpsE$wage.cut
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(0,1e+04]"] <- "$1-$10k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(1e+04,2e+04]"] <- "$10-$20k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(2e+04,3e+04]"] <- "$20-$30k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(3e+04,4e+04]"] <- "$30-$40k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(4e+04,5e+04]"] <- "$40-$50k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(5e+04,6e+04]"] <- "$50-$60k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(6e+04,7e+04]"] <- "$60-$70k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(7e+04,8e+04]"] <- "$70-$80k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(8e+04,9e+04]"] <- "$80-$90k"
# levels(cpsE$wageIncome)[levels(cpsE$wage.cut)=="(9e+04,1e+05]"] <- "$90-$100k"



#this is another unconditional mean by year 
yMeans <- cps %>% group_by(YEAR) %>% 
  summarise(n=n(), yMean=mean(wage09, na.rm=T))

cps$uMean09 <- NA
for(i in yMeans$YEAR){
  cps$uMean09[which(cps$YEAR==i)] <- yMeans$yMean[which(yMeans$YEAR==i)]
}
cps$deMeanedWage <- cps$wage09/cps$uMean09

#reduced
cps <- cps[,c(1,7:12,30:36)]
setwd(olddir)
write.csv(cps, file="cps.csv")
