#**
#*  @file     HSST_B2_DataScience_Practical
#*  @author   Gareth Price
#*  @date     10-05-2019
#*  @version  1.0
#*
#*  Playing with data from Oberije et al 2015 NSCLC survival model
#*

library(ggplot2)
library(naniar)
library(survival)
library(survminer)
library(haven)

#Read in Maastro data
#Need to separate data with semi-colon rather than standard comma and also check for whitespaces as NAs
lung.data = read.csv(file='H:\\Teaching\\HSST\\B2\\IntroducingDataScience\\Stage3_anonymized.csv',head=TRUE,sep=";",na.strings=c("NA",""," "))

#Explore missing data
gg_miss_var(lung.data,show_pct=TRUE)

#Remove those with >10% missing
ind=which((names(lung.data) == 'bmi') | (names(lung.data) == 'fev1pc_t0'))
lung.data=lung.data[,-ind]

#Convert from factor to numeric to allow interpolation
lung.data$tumorload_total=as.numeric((lung.data$tumorload_total))
lung.data$gtv1=as.numeric((lung.data$gtv1))

#Simple interpolation to median of data. Many more sophisticated approaches available
for(i in 1:ncol(lung.data)){
  if(sum(is.na(lung.data[,i]))>0){
    lung.data[is.na(lung.data[,i]),i]=median(lung.data[,i],na.rm=TRUE)
  }
}

#Data cleaning
#Change from comma separated decimal string to point separated
#Cast to correct type
testExpVars=c("gender","age","who3g","countpet_all6g","tstage","nstage","timing","group","eqd2","ott","gtv1")
lung.data.ind=lung.data[,which(names(lung.data) %in% testExpVars)]
lung.data.ind$gender=as.factor(lung.data.ind$gender)
lung.data.ind$age=as.numeric(gsub(',','.',lung.data.ind$age))
lung.data.ind$who3g=as.factor(lung.data.ind$who3g)
lung.data.ind$countpet_all6g=as.numeric(lung.data.ind$countpet_all6g)
lung.data.ind$tstage=as.factor(lung.data.ind$tstage)
lung.data.ind$nstage=as.factor(lung.data.ind$nstage)
lung.data.ind$timing=as.factor(lung.data.ind$timing)
lung.data.ind$group=as.numeric(lung.data.ind$group)
lung.data.ind$eqd2=as.numeric(gsub(',','.',lung.data.ind$eqd2))
lung.data.ind$ott=as.numeric(lung.data.ind$ott)
lung.data.ind$gtv1=as.numeric(gsub(',','.',lung.data.ind$gtv1))

lung.data.dep=lung.data[,which(names(lung.data) %in% c("survmonth","deadstat"))]
lung.data.dep$survmonth=as.numeric(gsub(',','.',lung.data.dep$survmonth))

#Build survival model and check survival curve
lung.survial=Surv(as.numeric(lung.data.dep$survmonth),lung.data.dep$deadstat)
ggsurvplot(survfit(lung.survial~1,data=lung.data.ind),data=lung.data,risk.table=TRUE)

#Univariable cox regression to find factors of potential interest for multivariable regression
for(i in 1:ncol(lung.data.ind)){
  print(as.formula(paste("lung.survial~",names(lung.data.ind)[i])))
  lung.uniCox=coxph(as.formula(paste("lung.survial~",names(lung.data.ind)[i])),data=lung.data.ind)
  print(summary(lung.uniCox))
}

#Include significant variables in multivariable regression to look for inter-relations
lung.multiCox=coxph(lung.survial~gender+age+who3g+countpet_all6g+nstage+timing+group+
                      eqd2+gtv1+ott,data=lung.data.ind)
print(summary(lung.multiCox))

#Dicotomise variables
lung.data.ind$ageUnder70=rep(0,nrow(lung.data.ind))
lung.data.ind$ageUnder70[which(lung.data.ind$age >70)]=1
lung.data.ind$ageUnder70=as.factor(lung.data.ind$ageUnder70)

lung.data.ind$whoPs=as.factor(lung.data.ind$who3g)
lung.data.ind$whoPs[which(as.numeric(lung.data.ind$whoPs) >=3)]=3

lung.data.ind$logGtv=log(lung.data.ind$gtv1)

lung.data.ind$plns=lung.data.ind$countpet_all6g
lung.data.ind$plns[which(as.numeric(lung.data.ind$plns) >=5)]=5

lung.data.ind$ottUnder28=rep(0,nrow(lung.data.ind))
lung.data.ind$ottUnder28[which(as.numeric(lung.data.ind$ott) > 28)]=1
lung.data.ind$ottUnder28=as.factor(lung.data.ind$ottUnder28)

lung.multiCox2=coxph(lung.survial~gender+ageUnder70+whoPs+plns+nstage+timing+group+eqd2+
                       logGtv+ottUnder28,data=lung.data.ind)
print(summary(lung.multiCox2))

#Plot models against dicotomous data
ggsurvplot(survfit(lung.survial~who3g,data=lung.data.ind),data=lung.data.ind,risk.table=TRUE)

#Backwards elimination using liklihood ratio test (does new model explain significantly different amounts of the variation in the data
#than the base model?)
anova(lung.multiCox2,coxph(lung.survial~whoPs+plns+timing+eqd2+ottUnder28,data=lung.data.ind))

