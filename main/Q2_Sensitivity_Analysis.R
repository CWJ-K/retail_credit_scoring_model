#-------------------------------------------------------------------------------------------------------------
#Description      : Industrial Analytics - Retail Credit Scoring  
#Authors          : Group 6 - Dung Tran,Jessie Zhu, Mirza Miftanula, Sabahat Zubair, Chen Wan Ju
#Last edited on   : 25 Februari 2018
#Edit comments    : 1.Data Preperation 
#                   2.Modeling:..........
#--------------------------------------------------------------------------------------------------------------

#Working Directory
getwd()
#setwd('C:/Users/Regem/Desktop')
setwd('C:/Users/Mirza Miftanula/Documents/UoE - Business Analytics/2nd Semester/Industrial Analytics/Project/R-Code')
rm(list=ls())
start_time = Sys.time()

#------------------------------------------1.Data Preparation---------------------------------------------------
# Import the dataset
if (!require('readxl')) { 
  install.packages('readxl')
  require(readxl)
}
df_metadata <- read_excel("data.xls")
df_metadata[] <- lapply(df_metadata, function(x){ if(is.character(x)) as.factor(x) else (x) })

# Check structure of the input df
  str(df_metadata)
  head(df_metadata)
# Check missing variables
  colSums(is.na(df_metadata)) ## Since missing value is on start_date then it can be ignored

# remove Start   &  AgmtNo  & DATASET     
  df_metadata$Start_Date<-NULL
  df_metadata$`AgmtNo`<-NULL

####### BINNING VARIABLES #############
# Classify 2 features into categorical type (as described in question)
  ## Age
     for (i in 1:length(df_metadata$AGE)){
      data = df_metadata$AGE[i]
      insertval = 0
      if (data <=30) {
        insertval = 0
      } else if (data<=40 & data >30 ) {
        insertval = 1
      } else if (data>40){
        insertval = 2
      }
      df_metadata$AGEBIN[i]=insertval
     }

  ## Income
     for (i in 1:length(df_metadata$MTHINCTH)){
      data = df_metadata$MTHINCTH[i]
      insertval = 0
      if (data <=5) {
        insertval = 0
      } else if (data<=8 & data >5 ) {
        insertval = 1
      } else if (data>8){
        insertval = 2
      }
      df_metadata$MTHINCTHBIN[i]=insertval
    }

# Check Unique Value for several features to evaluate whether it can be classed into several bins or not
  sapply(df_metadata,function(x){length(unique(x))})

# Check unique number for tenor and saldate_fraction
  sort(unique(df_metadata$TENORYR))
  sort(unique(df_metadata$SALDATFR))

# Split saldatfr into two bins with boundaries 0.5
  df_metadata$SALDTBIN <- sapply(df_metadata$SALDATFR,function(x){if (x <= 0.5) 0 else 1})

# Split tenor into four bins (<=1 year, 1-2 years, 2-3 years, >3 years)
  df_metadata$TENORBIN <- sapply(df_metadata$TENORYR,function(x){if (x <= 1) 0 else if (x<=2) 1 else if (x<=3) 2 else 3})


####### OUTLIERS TREATMENT #############
# Check for the data that contain some outliers (only numeric data)
  ##  Data Income 
      plot(df_metadata$MTHINCTH,main='Monthly Income',xlab="# of Customers",ylab="Salary_Amount")  ## It contains some outliers
      ### Define Interval Range
      upper <- quantile(df_metadata$MTHINCTH,probs = 0.75, na.rm = TRUE) + IQR(df_metadata$MTHINCTH, na.rm = TRUE)
      lower <- quantile(df_metadata$MTHINCTH,probs = 0.25, na.rm = TRUE) - IQR(df_metadata$MTHINCTH, na.rm = TRUE)
      ### add tagging to outlier data
      for (i in 1 : length(df_metadata$MTHINCTH)){
        datas = df_metadata[i,4]
        if (datas < lower | datas > upper){
          df_metadata$MTHINCTH_Outlier[i] <-1
        }else{
          df_metadata$MTHINCTH_Outlier[i] <-0
        }
      }
      ### Check number of outlier data in income
      length(which(df_metadata$MTHINCTH_Outlier==1))  
  ##  Data Down Payment
      plot(df_metadata$DWNPMFR,main='Down Payment Fraction',xlab="# of Customers",ylab=" Fraction of Down Payment") ## It does not contain outliers
  ##  Data Tenor
      plot(df_metadata$TENORYR,main='Tenure',xlab="# of Customers",ylab="Tenure (years)") ## It contains outlier
      ### Define Interval Range
      upper <- quantile(df_metadata$TENORYR,probs = 0.75, na.rm = TRUE) + IQR(df_metadata$TENORYR, na.rm = TRUE)
      lower <- quantile(df_metadata$TENORYR,probs = 0.25, na.rm = TRUE) - IQR(df_metadata$TENORYR, na.rm = TRUE)
      ### add tagging to outlier data
      for (i in 1 : length(df_metadata$TENORYR)){
        datas = df_metadata[i,6]
        if (datas < lower | datas > upper){
          df_metadata$TENORYR_Outlier[i] <-1
        }else{
          df_metadata$TENORYR_Outlier[i] <-0
        }
      }

# Check df which exclude outlier
  # df_metadata_ro <- df_metadata[which(df_metadata$TENORYR_Outlier==0|df_metadata$MTHINCTH_Outlier==0),]
  df_metadata_ro <- df_metadata
  str(df_metadata_ro)
  ## proportion default non default in remove outlier data
     length(which(df_metadata_ro$DefaulterFlag==1))/length(df_metadata_ro$DefaulterFlag)

#### PLOT AUTO CORRELATION ####
  correlation_mx <- round(cor(df_metadata_ro[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)]),2)
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("kassambara/ggcorrplot")
     
  library(ggcorrplot)
  ggcorrplot(correlation_mx,type='lower',hc.order = TRUE,outline.color = 'white')
     
### SET FINAL DATASET
  df_metadata_ro$Region<-NULL
  df_metadata_ro$ContractStatus<-NULL
  df_metadata_ro$AGEBIN<-NULL
  df_metadata_ro$SALDTBIN<-NULL
  df_metadata_ro$TENORBIN<-NULL
  df_metadata_ro$MTHINCTHBIN<-NULL
  df_metadata_ro$MTHINCTH_Outlier<-NULL
  df_metadata_ro$TENORYR_Outlier<-NULL
  
###### CREATE DUMMY VARIABLE ########
# One-hot ecoding 
  if (!require('caret')) { 
    install.packages('caret')
    require(caret)
  }
  library (caret)
  str(df_metadata_ro)  
  onehot<-data.frame(df_metadata_ro$Branch )  
  dmy <-dummyVars(" ~ .", data = onehot)
  data2 <- data.frame(predict(dmy, newdata =onehot ))
  df_metadata_ro <- cbind(df_metadata_ro,data2)
  df_metadata_ro$Branch<-NULL
  str(df_metadata_ro)



#------------------------------------------2.Data Modelling (Question 2) --------------------------------------------------- 
bulkdata<- df_metadata_ro
# Build Train_Data
  build_data <- bulkdata[which(bulkdata$DATASET=='BUILD'),]
# Build Test_Data
  validate_data <- bulkdata[which(bulkdata$DATASET=='VALIDATE'),]
# Check proportion of the defaulter in train and validation set
  length(which(build_data$DefaulterFlag==0))/length(build_data$DefaulterFlag)
  length(which(validate_data$DefaulterFlag==0))/length(validate_data$DefaulterFlag)
# Build the model
  detach (validate_data)
  attach(build_data)
  final_predictors_train <- data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,QUALHSC,
                                       QUAL_PG,SEXCODE,FULLPDC,FRICODE,WASHCODE,I(MTHINCTH*QUALHSC),I(AGE*QUAL_PG),
                                       DefaulterFlag,df_metadata_ro.Branch.Bangalore,df_metadata_ro.Branch.Chennai,
                                       df_metadata_ro.Branch.Coimbattore,df_metadata_ro.Branch.Ernakulam,
                                       df_metadata_ro.Branch.Kumbakonam,df_metadata_ro.Branch.Madurai,
                                       df_metadata_ro.Branch.Pondy,df_metadata_ro.Branch.Salem,
                                       df_metadata_ro.Branch.Tiruchy,df_metadata_ro.Branch.Tirunelvelli,
                                       df_metadata_ro.Branch.Tirupathi,df_metadata_ro.Branch.Vellore,
                                       df_metadata_ro.Branch.Vijayawada,df_metadata_ro.Branch.Vizag)
  
  final_model <- glm(DefaulterFlag~.,data = final_predictors_train, family = binomial("logit"),maxit=1000)
  summary(final_model)
  
# Make prediction
  detach(build_data)
  attach(validate_data)
  final_predictors_test <- data.frame(AGE,NOOFDEPE,MTHINCTH,SALDATFR,TENORYR,DWNPMFR,QUALHSC,
                                      QUAL_PG,SEXCODE,FULLPDC,FRICODE,WASHCODE,I(MTHINCTH*QUALHSC),I(AGE*QUAL_PG),
                                      DefaulterFlag,df_metadata_ro.Branch.Bangalore,df_metadata_ro.Branch.Chennai,
                                      df_metadata_ro.Branch.Coimbattore,df_metadata_ro.Branch.Ernakulam,
                                      df_metadata_ro.Branch.Kumbakonam,df_metadata_ro.Branch.Madurai,
                                      df_metadata_ro.Branch.Pondy,df_metadata_ro.Branch.Salem,
                                      df_metadata_ro.Branch.Tiruchy,df_metadata_ro.Branch.Tirunelvelli,
                                      df_metadata_ro.Branch.Tirupathi,df_metadata_ro.Branch.Vellore,
                                      df_metadata_ro.Branch.Vijayawada,df_metadata_ro.Branch.Vizag)
  
  final_prediction <- as.numeric(predict(final_model, final_predictors_test, type='response'))
  
# Plot the distribution of the probability of default
  par(mfrow=c(1,1))
  #plot(density(final_prediction),xlab='Probability',main = 'Density Plot of Output Scoring Model')
  #polygon(density(final_prediction),col='gray',border = 'black')
  hist(final_prediction,xlab='Probability',ylab='Freq',main = 'Histogram Plot of Output Scoring Model',col = 'grey',border = 'black')

# Calculate AUC
  if (!require('AUC')) { 
    install.packages('AUC')
    require(AUC)
  }
  library (AUC)
  plot(roc(final_prediction, as.factor(validate_data$DefaulterFlag)),main='ROC Curve Scoring Model (AUC = 69.6 %)')
  auc <- AUC::auc(roc(final_prediction, as.factor(validate_data$DefaulterFlag)))

# Calculate Different Threshold ##
##################################

# 1. Calculate LGD for individual customer
  source_data <- validate_data
  # Create function to calculate LGD
  for (i in 1:length(source_data$AGE)){
    LGD <- 0
    dwnpmt <- source_data$DWNPMFR[i]
    lp_cond <- 1/(1-dwnpmt)
    if (lp_cond <= 0.3){
      LGD <- 0.5 
    } else if (lp_cond > 1.4){
      LGD <- 0.4
    } else {
      LGD <- (1-(0.2*lp_cond/1.4))*0.5
    }
    source_data$INDLGD[i]<-LGD
  }
  # plot LGD
  hist(source_data$INDLGD,xlab='LGD',ylab='Freq',main = 'Histogram Plot of Individual LGD',col = 'grey',border = 'black')
   
  # Create function to calculate Expected Loan Amount
  for (i in 1:length(source_data$AGE)){
  monthly_payment <- source_data$MTHINCTH[i]
  term <- source_data$TENORYR[i]/12
  ELA <- monthly_payment*(1-(1+0.12)^(-1*term))/(0.12)
  source_data$ELA[i] <- ELA    
  }
  integrate()
  
  # plot ELA
  hist(source_data$INDLGD/source_data$ELA)
  
  # Add probability of default for each account to the df
  source_data$pd <- final_prediction
  hist(source_data$pd)
  quantile(source_data$pd)
  
  # Calculate Expected_Loss per account
  source_data$EL <- source_data$pd*source_data$ELA*source_data$INDLGD
  
  hist(source_data$EL)
  
 # 2. Create dataframe to store the value 
      rangeofcp <- seq(0,1,by=0.05)
      df_cp_comparison <- as.data.frame(matrix(seq(21*18),ncol = 18,nrow = length(rangeofcp)))
      names(df_cp_comparison)<-c('CP','11','10','01','00','LGD11','LGD10','LGD01','LGD00','Avg_PD_11','Avg_PD_10','AVG_PD_01',
                                 'AVG_PD_00','Estimate_Exposure_11','Estimate_Exposure_10',
                                 'Estimate_Exposure_01','Estimate_Exposure_00','Accuracy')
      data_output <- final_prediction
      defaulter_flag <- source_data$DefaulterFlag
      binary_output <- seq(1:length(data_output))
      indiv_lgd <- source_data$INDLGD
      indiv_ELA <- source_data$ELA
      indiv_EL <- source_data$EL
      indiv_PD <- source_data$pd
      df_classification <- as.data.frame(cbind(data_output,defaulter_flag,binary_output,indiv_lgd,indiv_ELA,indiv_EL,indiv_PD))
 # 3. Implement the cut off to the output
      for (i in 1:21){
      initial_cp <- rangeofcp[i]
      # Setting cut off with different CP
        #threshold <- quantile(df_classification$data_output,probs = seq((1-initial_cp),1))
        for (j in 1:length(df_classification$data_output)){
          if (df_classification$data_output[j]>=initial_cp){
            df_classification$binary_output [j]=1
          } else{
            df_classification$binary_output[j]=0
          }
        }
      # insert the data into test data
      if (!require('e1071')) { 
        install.packages('e1071')
        require('e1071') 
      }
      library(e1071)
      CF <- confusionMatrix(as.factor(df_classification$binary_output),as.factor(df_classification$defaulter_flag))
      Accuracy_val <- (CF$table[1]+CF$table[4])/sum(CF$table)
      df_cp_comparison[i,1]<- initial_cp
      df_cp_comparison[i,2]<- CF$table[4]
      df_cp_comparison[i,3]<- CF$table[2]
      df_cp_comparison[i,4]<- CF$table[3]
      df_cp_comparison[i,5]<- CF$table[1]
      df_cp_comparison[i,6]<- mean(df_classification$indiv_lgd[df_classification$defaulter_flag==1 & df_classification$binary_output==1])
      df_cp_comparison[i,7]<- mean(df_classification$indiv_lgd[df_classification$defaulter_flag==0 & df_classification$binary_output==1])
      df_cp_comparison[i,8]<- mean(df_classification$indiv_lgd[df_classification$defaulter_flag==1 & df_classification$binary_output==0])
      df_cp_comparison[i,9]<- mean(df_classification$indiv_lgd[df_classification$defaulter_flag==0 & df_classification$binary_output==0])
      df_cp_comparison[i,10]<- mean(df_classification$indiv_PD[df_classification$defaulter_flag==1 & df_classification$binary_output==1])
      df_cp_comparison[i,11]<- mean(df_classification$indiv_PD[df_classification$defaulter_flag==0 & df_classification$binary_output==1])
      df_cp_comparison[i,12]<- mean(df_classification$indiv_PD[df_classification$defaulter_flag==1 & df_classification$binary_output==0])
      df_cp_comparison[i,13]<- mean(df_classification$indiv_PD[df_classification$defaulter_flag==0 & df_classification$binary_output==0])
      df_cp_comparison[i,14]<- sum(df_classification$indiv_ELA[df_classification$defaulter_flag==1 & df_classification$binary_output==1])
      df_cp_comparison[i,15]<- sum(df_classification$indiv_ELA[df_classification$defaulter_flag==0 & df_classification$binary_output==1])
      df_cp_comparison[i,16]<- sum(df_classification$indiv_ELA[df_classification$defaulter_flag==1 & df_classification$binary_output==0])
      df_cp_comparison[i,17]<- sum(df_classification$indiv_ELA[df_classification$defaulter_flag==0 & df_classification$binary_output==0])
      df_cp_comparison[i,18]<-Accuracy_val
      }
      
      write.table(df_cp_comparison,file="DF_Comparison.csv",append=F,sep=",",col.names=T,row.names=F)
      