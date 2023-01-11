# Code from Chapter 10 of R Companion for Sampling: Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Contingency Tables and Odds Ratios ##########

##### Example 10.1

### Multinomial sampling 
# create the categorical table (Table 10.1)
cablecomp<-matrix(c(119,188,88,105), ncol=2, byrow=2)
dimnames(cablecomp)<-list(Cable=c("yes", "no"),
                        Computer=c("yes","no"))
cablecomp

# Pearson's chi-square test under multinomial sampling, obtain X^2
cablechisq<-chisq.test(cablecomp,correct=F)
cablechisq
# Expected values under null hypothesis
cablechisq$expected
# odds ratio
(cablecomp[1,1]/cablecomp[1,2])/(cablecomp[2,1]/cablecomp[2,2])

##### Example 10.5

### Contingency tables for data from a complex survey
data(syc)
dsyc<-svydesign(ids=~psu,weights=~finalwt,strata=~stratum,nest=TRUE,data=syc)
dsyc # Verify this is a stratified cluster sample
# Create contingency table by incorporating weights
example1005 <- svytable(~famtime+everviol,design=dsyc)
example1005
# Perform the Wald chi-square test
summary(example1005,statistic="Wald")
# Alternatively, can calculate the Wald statistic directly using svychisq
# without forming the table first
svychisq(~famtime+everviol,design=dsyc,statistic="Wald")


########## Chi-Square Tests ##########

##### Example 10.6

# Create variables currviol and ageclass for 10.6
syc$currviol <- syc$crimtype
syc$currviol[syc$crimtype != 1 | is.na(syc$crimtype)] <- 0

syc$ageclass <- syc$age
syc$ageclass[syc$age <= 15] <- 1
syc$ageclass[syc$age == 16 | syc$age == 17] <- 2
syc$ageclass[18 <= syc$age] <- 3

# Specify the survey design
dsyc<-svydesign(ids=~psu,weights=~finalwt,strata=~stratum,nest=TRUE,data=syc)
# estimate the contingency table
svytable(~currviol+ageclass,design=dsyc)
# First-order Rao-Scott test
svychisq(~currviol+ageclass,design=dsyc,statistic="Chisq")
# Second-order Rao-Scott test 
# (this is the default, can also request with "statistic=F")
svychisq(~currviol+ageclass, design=dsyc)

### Obtain design effects
# deffs for table cells
svymean(~interaction(factor(ageclass), factor(currviol)),design=dsyc,deff="replace")
# deffs for table margins
svymean(~factor(ageclass)+ factor(currviol),design=dsyc,deff="replace")


########## Loglinear Models ##########

##### Example 10.8

cabletable <-  matrix(c(
 "no","no",	105,
 "no","yes",	88,
 "yes", "no",	188,
 "yes", "yes",	119),byrow=T,nrow=4,ncol=3)
colnames(cabletable) <- c("cable","computer","count")
cabletable <- data.frame(cabletable)
cabledf <- cabletable[rep(row.names(cabletable), cabletable[,3]), 1:2]
dim(cabledf)
cabledf$wt <- rep(1,500)
head(cabledf)
dcable <- svydesign(id=~1, weights=~wt, data=cabledf)

# chi-squared test for independent data, no continuity correction
chisq.test(cabledf$computer,cabledf$cable,correct=F)
svychisq(~computer+cable,design=dcable,statistic="Chisq")

# Fit loglinear model for independence, with additive factors
cableindep <- svyloglin(~factor(cable)+factor(computer),design=dcable)
summary(cableindep)
# obtain coefficients including intercept, deviance
cableindep$model
# obtain the predicted counts under the independence model
cableindep$model$fitted.values
# obtain the fitted probabilities under the model
cableindep$model$fitted.values/500

# Fit saturated loglinear model 
cablesat <- svyloglin(~factor(cable)*factor(computer),design=dcable)
summary(cablesat)
# this can also be obtained by comparing the two models
anova(cablesat,cableindep)

##### Example 10.9


data(syc)
syctable3way <- svytable(~ageclass+everviol+famtime,design=dsyc)
syctable3way

# Estimate probabilities in table
syctable3way/sum(syctable3way)

# Define design object
syc$currviol <- syc$crimtype
syc$currviol[syc$crimtype != 1 | is.na(syc$crimtype)] <- 0
syc$ageclass <- syc$age
syc$ageclass[syc$age <= 15] <- 1
syc$ageclass[syc$age == 16 | syc$age == 17] <- 2
syc$ageclass[18 <= syc$age] <- 3
dsyc<-svydesign(ids=~psu,weights=~finalwt,strata=~stratum,nest=TRUE,data=syc)

# Fit saturated loglinear model
sycsat <- svyloglin(~ageclass*everviol*famtime,design=dsyc)
summary(sycsat)

# Fit additive loglinear model for independent factors
sycind <- svyloglin(~ageclass+everviol+famtime,design=dsyc)
summary(sycind)
# compare independent and saturated models
anova(sycsat, sycind)



















