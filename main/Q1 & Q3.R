##########################################################################################################
#  Logistic Regression
###########################################################################################################
#install.packages("SparseM")
install.packages("stargazer")
library(stargazer)
#install.packages("AER")
library(AER)
library(SparseM)
library(ggplot2)
library(Formula)
library(lattice)
library(survival)
library(Hmisc)
library(rms)
# Pseudo R-squared
#install.packages("DescTools")
library(DescTools)
#install.packages("stargazer")
library(stargazer)
#robust
library(plyr)
library(MASS)
#install.packages("robustbase")
library(robustbase)
#install.packages("mfx") 
library(zoo)
library(mfx)

rm(list= ls())
#### 1. Check basetable & Split the data ####
#############################################
setwd('C:/Users/R/Desktop/Stata')
df_metadata<-read.csv("Data.csv", header = TRUE)

colnames(df_metadata)
table(df_metadata$DefaulterType )

#cor(WASHCODE,FRICODE)

#detach(df_metadata)
attach(df_metadata)
# original variables  ==> can not use in logistic
Originaldf =data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,QUAL_PG,SEXCODE,FULLPDC,FRICODE,WASHCODE,DefaulterFlag,DefaulterType,
                  AGEBIN,MTHINCTHBIN,SALDTBIN,TENORBIN,df_metadata..Contract.Status..Closed,df_metadata..Contract.Status..Foreclosed,df_metadata..Contract.Status..Live,df_metadata..Contract.Status..Seized,df_metadata.Region.AP1,
                  df_metadata.Region.AP2,df_metadata.Region.Chennai,df_metadata.Region.KA1,df_metadata.Region.KE2,df_metadata.Region.TN1,df_metadata.Region.TN2,df_metadata.Region.Vellore,df_metadata.Branch.Bangalore,df_metadata.Branch.Chennai,
                  df_metadata.Branch.Coimbattore,df_metadata.Branch.Ernakulam,df_metadata.Branch.Kumbakonam,df_metadata.Branch.Madurai,df_metadata.Branch.Pondy,df_metadata.Branch.Salem,
                  df_metadata.Branch.Tiruchy,df_metadata.Branch.Tirunelvelli,df_metadata.Branch.Tirupathi,df_metadata.Branch.Vellore,df_metadata.Branch.Vijayawada,df_metadata.Branch.Vizag)

#remove:AGEBIN,SALDTBIN,TENORBIN,MTHINCTHBIN,DefaulterType, region, DefaulterFlag
# Original model
df1 =data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,QUAL_PG,SEXCODE,FULLPDC,FRICODE,WASHCODE,DefaulterFlag,
                  df_metadata.Branch.Bangalore,df_metadata.Branch.Chennai,df_metadata.Branch.Coimbattore,df_metadata.Branch.Ernakulam,df_metadata.Branch.Kumbakonam,df_metadata.Branch.Madurai,df_metadata.Branch.Pondy,df_metadata.Branch.Salem,
                  df_metadata.Branch.Tiruchy,df_metadata.Branch.Tirunelvelli,df_metadata.Branch.Tirupathi,df_metadata.Branch.Vellore,df_metadata.Branch.Vijayawada,df_metadata.Branch.Vizag)

# model: take log(MTHINCTH)
df2 =data.frame(AGE,NOOFDEPE,log(MTHINCTH),SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,QUAL_PG,SEXCODE,FULLPDC,FRICODE,WASHCODE,DefaulterFlag,
                df_metadata.Branch.Bangalore,df_metadata.Branch.Chennai,df_metadata.Branch.Coimbattore,df_metadata.Branch.Ernakulam,df_metadata.Branch.Kumbakonam,df_metadata.Branch.Madurai,df_metadata.Branch.Pondy,df_metadata.Branch.Salem,
                df_metadata.Branch.Tiruchy,df_metadata.Branch.Tirunelvelli,df_metadata.Branch.Tirupathi,df_metadata.Branch.Vellore,df_metadata.Branch.Vijayawada,df_metadata.Branch.Vizag)

#model2: interaction:Income& education, Age & education, Gender& profession
# find taking log does not affect ==> X log
df3 =data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,I(MTHINCTH*QUALHSC),I(AGE*QUALHSC),QUAL_PG,I(MTHINCTH*QUAL_PG),I(AGE*QUAL_PG),
                SEXCODE,I(SEXCODE*PROFBUS),FULLPDC,FRICODE,WASHCODE,DefaulterFlag,
                df_metadata.Branch.Bangalore,df_metadata.Branch.Chennai,df_metadata.Branch.Coimbattore,df_metadata.Branch.Ernakulam,df_metadata.Branch.Kumbakonam,df_metadata.Branch.Madurai,df_metadata.Branch.Pondy,df_metadata.Branch.Salem,
                df_metadata.Branch.Tiruchy,df_metadata.Branch.Tirunelvelli,df_metadata.Branch.Tirupathi,df_metadata.Branch.Vellore,df_metadata.Branch.Vijayawada,df_metadata.Branch.Vizag)


####Question 1####
LR1 <- glm(DefaulterFlag~., data = df1, family = binomial("logit"),maxit=1000)
LR2 <- glm(DefaulterFlag~., data = df2, family = binomial("logit"),maxit=1000)
LR3 <- glm(DefaulterFlag~., data = df3, family = binomial("logit"),maxit=1000)
stargazer(LR1,LR2,LR3,type="text",title="Table1: Summary Statistics of Binary Logistic Regressions", out="Q1.html")




#####Question 3####
DefaulterType <- as.factor(mapvalues(DefaulterType, 
                                                 from=c("0","1","2"),
                                                 to=c("0","1","2")))

#ineraction
df4 =data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,I(MTHINCTH*QUALHSC),I(AGE*QUALHSC),QUAL_PG,I(MTHINCTH*QUAL_PG),I(AGE*QUAL_PG),
                SEXCODE,I(SEXCODE*PROFBUS),FULLPDC,FRICODE,WASHCODE,DefaulterType,
                df_metadata.Branch.Bangalore,df_metadata.Branch.Chennai,df_metadata.Branch.Coimbattore,df_metadata.Branch.Ernakulam,df_metadata.Branch.Kumbakonam,df_metadata.Branch.Madurai,df_metadata.Branch.Pondy,df_metadata.Branch.Salem,
                df_metadata.Branch.Tiruchy,df_metadata.Branch.Tirunelvelli,df_metadata.Branch.Tirupathi,df_metadata.Branch.Vellore,df_metadata.Branch.Vijayawada,df_metadata.Branch.Vizag)




MLR1<-polr(DefaulterType~.,data = df4, Hess=TRUE)


stargazer(MLR1,type="text",title="Table3: Summary Statistics of Ordered Logistic Regressions", out="Q3.html")


