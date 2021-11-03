##########################################################################################################
#  Logistic Regression: Hausman Test
###########################################################################################################
#install.packages("SparseM")
library(SparseM)
library(ggplot2)
library(Formula)
library(Hmisc)
library(lattice)
library(survival)
library(rms)
# Pseudo R-squared
#install.packages("DescTools")
library(DescTools)
#install.packages("stargazer")
library(stargazer)
#Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#R package version 5.2.2. https://CRAN.R-project.org/package=stargazer
#install.packages('bife')
library(bife)
library(plm)
##
library(Matrix)
library(lme4)
rm(list= ls())

library(plyr)
#### 1. Check basetable & Split the data ####
#############################################
setwd('C:/Users/R/Desktop')

Oridata<-read.csv("OriginalData.csv")

colnames(Oridata)

panel<-pdata.frame(Oridata, index=c("AgmtNo", "Start_Date"))
attach(Oridata)

X<-cbind(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,PROFBUS,QUALHSC,QUAL_PG,SEXCODE,FULLPDC,FRICODE)


# Test Random effects and Fixed effects

result_fe <- glm(DefaulterFlag~X+factor(Branch), data = Oridata,family = binomial("logit"))

AGE1<-scale(AGE)
result_re <- glmer(DefaulterFlag~AGE1+NOOFDEPE+MTHINCTH+SALDATFR+TENORYR+DWNPMFR+PROFBUS+QUALHSC+QUAL_PG+SEXCODE+FULLPDC+FRICODE+(1|Branch), data = Oridata,family = binomial("logit"),control = glmerControl(optimizer = "bobyqa"))

phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)  ## changed coef() to fixef() for glmer
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",  alternative = alternative,
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}


phtest_glmer(result_re,result_fe)
