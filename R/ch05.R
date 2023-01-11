# Code from Chapter 5 of R Companion for Sampling: 
# Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Estimates from One-Stage Cluster Samples ##########

##### Example 5.2

data(gpa)
# define one-stage cluster design
# note that id is suite instead of individual student as we take an SRS of suites
dgpa<-svydesign(id=~suite,weights=~wt,fpc=~rep(100,20),data=gpa) 
dgpa  
# estimate mean and se
gpamean<-svymean(~gpa,dgpa)
gpamean
degf(dgpa)
# n=5, t-approximation is suggested for CI
confint(gpamean,level=.95,df=4) # use t-approximation
# confint(gpamean,level=.95) # uses normal approximation, if desired (for large n) 
# estimate total and se (if desired)
gpatotal<-svytotal(~gpa,dgpa)
gpatotal
confint(gpatotal,level=.95,df=4)

# you can also calculate SEs by direct formula
suitesum<-tapply(gpa$gpa,gpa$suite,sum)  #sum gpa for each suite
# variability comes from among the suites
st2<-var(suitesum) 
st2
# SE of t-hat, formula (5.3) of SDA
vthat <-100^2*(1-5/100)*st2/5
sqrt(vthat) 
# SE of ybar, formula (5.6) of SDA
sqrt(vthat)/(4*100)

##### Example 5.6

data(algebra)
algebra$sampwt<-rep(187/12,299)
# define one-stage cluster design
dalg<-svydesign(id=~class,weights=~sampwt,fpc=~rep(187,299), data=algebra) 
dalg
# estimate mean and se
svymean(~score,dalg)
# n=12, t-distribution is suggested for CI
degf(dalg)
confint(svymean(~score,dalg),level=.95,df=11) #use t-approximation
# estimate total and se if desired
svytotal(~score,dalg)
confint(svytotal(~score,dalg),level=.95,df=11)

########## Estimates from Multi-Stage Cluster Samples ##########

##### Example 5.8

data(coots)
# Want to estimate the mean egg volume
nrow(coots) #368
coots$ssu<-rep(1:2,184) # index of ssu
coots$relwt<-coots$csize/2
head(coots)
dcoots<-svydesign(id=~clutch+ssu,weights=~relwt,data=coots)
dcoots
svymean(~volume,dcoots)  #ratio estimator
confint(svymean(~volume,dcoots),level=.95,df=183)
# now only include psu information, results are the same
dcoots2<-svydesign(id=~clutch,weights=~relwt,data=coots)
dcoots2
svymean(~volume,dcoots2)  

##### Example 5.7

### With-replacement variance

data(schools)
head(schools)
# calculate with-replacement variance; no fpc argument
# include psu variable in id; include weights
dschools<-svydesign(id=~schoolid,weights=~finalwt,data=schools)
# dschools tells you this is treated as a with-replacement sample
dschools
mathmean<-svymean(~math,dschools)
mathmean
degf(dschools) 
# use t distribution for confidence intervals because there are only 10 psus
confint(mathmean,df=degf(dschools))
# estimate proportion and total number of students with mathlevel=2
svymean(~factor(mathlevel),dschools)
svytotal(~factor(mathlevel),dschools)

### Without-replacement variance

# create a variable giving each student an id number
schools$studentid<-1:(nrow(schools))
# calculate without-replacement variance 
# specify both stages of the sample in the id argument
# give both sets of population sizes in the fpc argument
# do not include the weight argument
dschoolwor<-svydesign(id=~schoolid+studentid,fpc=~rep(75,nrow(schools))+Mi,
                       data=schools)
dschoolwor
mathmeanwor<-svymean(~math,dschoolwor)
mathmeanwor
confint(mathmeanwor,df=degf(dschoolwor))
# estimate proportion and total number of students with mathlevel=2
svymean(~factor(mathlevel),dschoolwor)
svytotal(~factor(mathlevel),dschoolwor)


########## Model-Based Design and Analysis for Cluster Samples ##########

##### Example 5.12

# run lm with schoolid as a factor 
fit5.12<-lm(math~factor(schoolid), data=schools)
# print ANOVA table
anova(fit5.12)
# extract the value of R-squared and adjusted R-squared
summary(fit5.12)$r.squared 
summary(fit5.12)$adj.r.squared 

##### Example 5.14

library(nlme)
fit5.14 <- lme(fixed=math~1,random=~1|factor(schoolid),data=schools)
summary(fit5.14)
# extract the variance components
VarCorr(fit5.14)

##### Exercise 5.40

set.seed(9231)
# generate intervals for cluster sample with groupcorr = 0.3
intervals_ex40(groupcorr = 0.3) # leave other parameters unchanged
