# Code from Chapter 8 of R Companion for Sampling: 
# Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## How R Functions Treat Missing Data ##########

data(impute)
impute$crime
is.na(impute$crime) # vector with TRUE for missing values
# identify the rows with no missing values in columns 5-6
impute$cc<-complete.cases(impute[,5:6]) 
impute$cc
mean(impute$crime)  # returns NA
mean(impute$crime,na.rm=TRUE) # calculates mean of non-missing values
table(impute$crime,impute$violcrime) # excludes values missing in either variable
table(impute$crime,impute$violcrime,useNA="ifany") # counts NAs as category in table
# input design information, use relative weights of 1 for comparison with above
dimpute <- svydesign(id = ~1, weights = rep(1,20), data = impute)
dimpute
# calculate survey mean and se
svymean(~crime,dimpute) # returns NA
svymean(~crime, dimpute, na.rm=TRUE)
svytable(~violcrime+crime,dimpute)

########## Poststratification and Raking ##########

rakewtsum <- data.frame(gender=rep(c("F","M"),each=5),
                       race=rep(c("Black","White","Asian","NatAm","Other"),times=2),
                       wtsum=c(300,1200,60,30,30,150,1080,90,30,30))
rakewtsum # check data entry
# Need data frame with individual records to use rake function
rakedf <- rakewtsum[rep(row.names(rakewtsum), rakewtsum[,3]/6), 1:2]
dim(rakedf)
rakedf$wt <- rep(6,nrow(rakedf))
# Create the survey design object
drake <- svydesign(id=~1, weights=~wt, data=rakedf)
# Create data frames containing the marginal counts
pop.gender <- data.frame(gender=c("F","M"), Freq=c(1510,1490))
pop.race <- data.frame(race=c("Black","White","Asian","NatAm","Other"), 
                       Freq=c(600,2120,150,100,30))
# Now create survey design object with raked weights
drake2 <- rake(drake, list(~gender,~race), list(pop.gender, pop.race))
drake2 # describes SRS with replacement
# Look at first 10 entries in vector of raked weights
weights(drake2)[1:10]
# Look at sum of raked weights for raking cells
svytable(~gender+race, drake2)
# Look at sum of raked weights for margins
svytotal(~factor(gender),drake2)
svytotal(~factor(race),drake2)

########## Imputation ##########

##### Example 8.9

##### cell mean imputation #####
impute$education
impute.cm<-impute
# define matrix giving imputation flags, TRUE for each missing value
impute.flag<-is.na(impute)
# fit two-way model with interaction, omit NAs from model-fitting
edmodel<-lm(education~factor(gender)*factor(age>=35),
            data=impute.cm,na.action=na.omit)
# replace missing values with imputations from model
newdata<- impute[is.na(impute$education),]
impute.cm$education[is.na(impute$education)] <- predict(edmodel,newdata)
impute.cm$education

##### For most imputation applications, we recommend using one of the R packages described in the text





