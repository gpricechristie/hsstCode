#**
#*  @file     HSST_B2_DataScience_Practical
#*  @author   Gareth Price
#*  @date     10-05-2019
#*  @version  1.0
#*
#*  Playing with data from Oberije et al 2015 NSCLC survival model
#*

#Install packages if needed
install.packages("ggplot2")
install.packages("naniar")
install.packages("survival")
install.packages("survminer")
install.packages("haven")
install.packages("formattable")

#Import libraries
library(ggplot2)
library(naniar)
library(survival)
library(survminer)
library(haven)
library(formattable)

#Read in Maastro data
#Need to separate data with semi-colon rather than standard comma and also check for whitespaces as NAs
lung.data = read.csv(file='Stage3_anonymized.csv',head=TRUE,sep=";",na.strings=c("NA",""," "))

#Explore missing data
names(lung.data)
head(lung.data)
print('Number of rows')
nrow(lung.data)

gg_miss_var(lung.data,show_pct=TRUE)

#Remove those with >10% missing
ind=which((names(lung.data) == 'bmi') | (names(lung.data) == 'fev1pc_t0'))
lung.data=lung.data[,-ind]
gg_miss_var(lung.data,show_pct=TRUE)

#Data cleaning
#Independent (explanatory) factors
#Change from comma separated decimal string to point separated
#Cast to correct type
testExpVars=c("gender","age","who3g","countpet_all6g","tstage","nstage","timing","group","eqd2","ott","gtv1")
lung.data.ind=lung.data[,which(names(lung.data) %in% testExpVars)]
head(lung.data.ind)

#Imputation
#Convert from factor to numeric to allow simple imputation
lung.data.ind$age=as.numeric(gsub(',','.',lung.data.ind$age))
lung.data.ind$eqd2=as.numeric(gsub(',','.',lung.data.ind$eqd2))
lung.data.ind$gtv1=as.numeric(gsub(',','.',lung.data.ind$gtv1))
head(lung.data.ind)

#Simple imputation to median of data. Many more sophisticated approaches available
for(i in 1:ncol(lung.data.ind)){
  if(sum(is.na(lung.data.ind[,i]))>0){
    lung.data.ind[is.na(lung.data.ind[,i]),i]=median(lung.data.ind[,i],na.rm=TRUE)
  }
}

#Look at missing data now
gg_miss_var(lung.data.ind)

#Order categorical data is needed (into ordinalformat)
lung.data.ind$gender=factor(lung.data.ind$gender)
lung.data.ind$who3g=factor(lung.data.ind$who3g, order=T, levels=c(0,1,2,3,4))
lung.data.ind$tstage=factor(lung.data.ind$tstage, order=T, levels=c(0,1,2,3,4))
lung.data.ind$nstage=factor(lung.data.ind$nstage, order=T, levels=c(0,1,2,3,4))
lung.data.ind$timing=factor(lung.data.ind$timing)
head(lung.data.ind)

#Dependent factors (survival)
lung.data.dep=lung.data[,which(names(lung.data) %in% c("survmonth","deadstat"))]
lung.data.dep$survmonth=as.numeric(gsub(',','.',lung.data.dep$survmonth))
head(lung.data.dep)

#Build survival model and check survival curve
lung.survial=Surv(as.numeric(lung.data.dep$survmonth),lung.data.dep$deadstat)
ggsurvplot(survfit(lung.survial~1,data=lung.data.ind),data=lung.data,risk.table=TRUE)

#Univariable cox regression to find factors of potential interest for multivariable regression
lung.uniCox=coxph(lung.survial~gtv1,data=lung.data.ind)
print(summary(lung.uniCox))

#Plot data dichotomised on GTV median
lung.data.dichot=lung.data.ind
lung.data.dichot$gtvRisk = rep(0,nrow(lung.data.ind))
lung.data.dichot$gtvRisk[which(lung.data.dichot$gtv1 > median(lung.data.dichot$gtv1))]=1
ggsurvplot(survfit(lung.survial~gtvRisk,data=lung.data.dichot),data=lung.data.dichot,risk.table=TRUE)

#Univariable analysis over all in data frame
analysis.results = data.frame(variable=character(ncol(lung.data.ind)),univariableHR=numeric(ncol(lung.data.ind)),
                              univariableP=numeric(ncol(lung.data.ind)),stringsAsFactors = FALSE)
for(i in 1:ncol(lung.data.ind)){
  lung.uniCox=coxph(as.formula(paste("lung.survial~",names(lung.data.ind)[i])),data=lung.data.ind)
  analysis.results$variable[i]=toString(names(lung.data.ind)[i])
  analysis.results$univariableHR[i]=summary(lung.uniCox)$coefficients[1,2]
  analysis.results$univariableP[i]=summary(lung.uniCox)$coefficients[1,5]
}
formattable(analysis.results)

#Include significant variables in multivariable regression to look for inter-relations
lung.multiCox=coxph(lung.survial~gender+age+who3g+countpet_all6g+tstage+nstage+timing+group+
                      eqd2+ott+gtv1,data=lung.data.ind)
print(lung.multiCox)

#Add to table for display
multivariableHR=numeric(ncol(lung.data.ind))
multivariableP=numeric(ncol(lung.data.ind))

multivariableHR[1]=summary(lung.multiCox)$coefficients[1,2]
multivariableP[1]=summary(lung.multiCox)$coefficients[1,5]

multivariableHR[2]=summary(lung.multiCox)$coefficients[2,2]
multivariableP[2]=summary(lung.multiCox)$coefficients[2,5]

multivariableHR[3]=summary(lung.multiCox)$coefficients[3,2]
multivariableP[3]=summary(lung.multiCox)$coefficients[3,5]

multivariableHR[4]=summary(lung.multiCox)$coefficients[7,2]
multivariableP[4]=summary(lung.multiCox)$coefficients[7,5]

multivariableHR[5]=summary(lung.multiCox)$coefficients[8,2]
multivariableP[5]=summary(lung.multiCox)$coefficients[8,5]

multivariableHR[6]=summary(lung.multiCox)$coefficients[12,2]
multivariableP[6]=summary(lung.multiCox)$coefficients[12,5]

multivariableHR[7]=summary(lung.multiCox)$coefficients[16,2]
multivariableP[7]=summary(lung.multiCox)$coefficients[16,5]

multivariableHR[8]=summary(lung.multiCox)$coefficients[18,2]
multivariableP[8]=summary(lung.multiCox)$coefficients[18,5]

multivariableHR[9]=summary(lung.multiCox)$coefficients[19,2]
multivariableP[9]=summary(lung.multiCox)$coefficients[19,5]

multivariableHR[10]=summary(lung.multiCox)$coefficients[20,2]
multivariableP[10]=summary(lung.multiCox)$coefficients[20,5]

multivariableHR[11]=summary(lung.multiCox)$coefficients[21,2]
multivariableP[11]=summary(lung.multiCox)$coefficients[21,5]

analysis.results$multivariableHR=multivariableHR
analysis.results$multivariableP=multivariableP

formattable(analysis.results)
