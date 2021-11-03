rm(list=ls())

library(readxl)
data <- read_excel("C:/Users/Asus/Desktop/MSc BA/Semester 2/Industrial Analytics/Term Project/data4.xlsx")
View(data)
class(data)
names(data)
str(data)

summary(data)

# Change categorical variables to factors
categories <- c("ContractStatus", "NOOFDEPE", "PROFBUS", "QUALHSC", "QUAL_PG","SEXCODE", "FULLPDC", "FRICODE", "WASHCODE", "Region",
                "Branch", "DefaulterFlag")


data$DefaulterFlag <- as.factor(data$DefaulterFlag)
data$ContractStatus <- as.factor(data$ContractStatus)
data$NOOFDEPE <- as.factor(data$NOOFDEPE)
data$PROFBUS <- as.factor(data$PROFBUS)
data$QUALHSC <- as.factor(data$QUALHSC)
data$QUAL_PG <- as.factor(data$QUAL_PG)
data$SEXCODE <- as.factor(data$SEXCODE)
data$FULLPDC <- as.factor(data$FULLPDC)
data$FRICODE <- as.factor(data$FRICODE)
data$WASHCODE <- as.factor(data$WASHCODE)
data$Region <- as.factor(data$Region)
data$Branch <- as.factor(data$Branch)


data$Education <- ifelse((data$QUALHSC==1 & data$QUAL_PG==0),'HS',ifelse((data$QUALHSC==0 & data$QUAL_PG==0),'UG','PG'))
data$Age_bin <- ifelse(data$AGE<30,'< 30',ifelse((data$AGE>=30 & data$AGE<40),'30-40','> 40'))
data$Income <- ifelse(data$MTHINCTH<5,'< 5k',ifelse((data$MTHINCTH>=5 & data$MTHINCTH<8),'5k-8k','> 8k'))
data$SalaryDate <- ifelse(data$SALDATFR<=0.5, 'Before 15', 'After 15')
data$DownPayment <- ifelse(data$DWNPMFR<=0.5, '<= 0.5', '> 0.5')
data$Asset <- ifelse((data$FRICODE==1 & data$WASHCODE==0),'Fridge',ifelse((data$FRICODE==0 & data$WASHCODE==1),'Washing Machine',ifelse((data$FRICODE==1 & data$WASHCODE==1),'Both','None')))


tenure_bin<-function(tenure){
  if (tenure>=0 & tenure< 1){
    return('< 1 Year')
  }else if (tenure>=1 & tenure<2){
    return('1-2 Year')
  }else if (tenure>=2 & tenure<3){
    return ('2-3 Year')
  }else{
    return ('3-4 Year')
  }}
data$Tenure<-sapply(data$TENORYR, tenure_bin)

data$Education <- as.factor(data$Education)
data$Age_bin <- as.factor(data$Age_bin)
data$Income <- as.factor(data$Income)
data$SalaryDate <- as.factor(data$SalaryDate)
data$DownPayment <- as.factor(data$DownPayment)
data$Asset <- as.factor(data$Asset)
data$Tenure <- as.factor(data$Tenure)


data$Inc_Edu <- ifelse((data$Income =='< 5k' & data$Education == 'HS'),'HS & < 5k',
                       ifelse((data$Income =='5k-8k' & data$Education == 'HS'),'HS & 5-8k',
                              ifelse((data$Income =='> 8k' & data$Education == 'HS'),'HS & 8k-Max', 
                                     ifelse((data$Income =='< 5k' & data$Education == 'UG'),'UG & <5k',
                                            ifelse((data$Income =='5k-8k' & data$Education == 'UG'),'UG & 5-8k',
                                                   ifelse((data$Income =='> 8k' & data$Education == 'UG'),'UG & 8k-Max',
                                                          ifelse((data$Income =='< 5k' & data$Education == 'PG'),'PG & <5k',
                                                                 ifelse((data$Income =='5k-8k' & data$Education == 'PG'),'PG & 5-8k','PG & 8k-Max'))))))))

data$Inc_Edu <- as.factor(data$Inc_Edu)

install.packages("ggplot2")
install.packages("gridExtra")
require(ggplot2)
require(gridExtra)


# Visualize the variables
# In this part, we focus on the proportion of defaulter ad non-defalter in each group of each variable
p1 <- ggplot(data, aes(x = ContractStatus, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Contract Status") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p2 <- ggplot(data, aes(x = NOOFDEPE, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("No. of Dependant") + theme(legend.position="none") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p3 <- ggplot(data, aes(x = PROFBUS, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Professional(0) vs Business(1)") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p4 <- ggplot(data, aes(x = QUALHSC, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Qualification of HSC") + geom_bar(aes(fill=DefaulterFlag),position = "fill")

p5 <- ggplot(data, aes(x = QUAL_PG, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Qualification of PG") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p6 <- ggplot(data, aes(x = SEXCODE, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Female(0) vs Male(1)") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p7 <- ggplot(data, aes(x = FULLPDC, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Full posted-date check") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p8 <- ggplot(data, aes(x = FRICODE, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Fridge") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p9 <- ggplot(data, aes(x = WASHCODE, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Washing Machine") + geom_bar(aes(fill=DefaulterFlag),position = "fill")

p10 <- ggplot(data, aes(x = Region, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Region") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p11 <- ggplot(data, aes(x = Branch, ..count..)) + xlab(NULL) + ylab("Proportion") + theme(legend.position="none") + ggtitle("Branch") + geom_bar(aes(fill=DefaulterFlag),position = "fill")

p12 <- ggplot(data, aes(x = Education, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Education") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p13 <- ggplot(data, aes(x = Age_bin, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Age") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p14 <- ggplot(data, aes(x = Income, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Income") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p15 <- ggplot(data, aes(x = SalaryDate, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Salary Date") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p16 <- ggplot(data, aes(x = DownPayment, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Down Payment") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p17 <- ggplot(data, aes(x = Asset, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Fridge & Washing Machine") + geom_bar(aes(fill=DefaulterFlag),position = "fill")
p18 <- ggplot(data, aes(x = Tenure, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Tenure") + geom_bar(aes(fill=DefaulterFlag),position = "fill")

p19 <- ggplot(data, aes(x = Inc_Edu, ..count..)) + xlab(NULL) + ylab("Proportion") + ggtitle("Income & Education") + geom_bar(aes(fill=DefaulterFlag),position = "fill")

