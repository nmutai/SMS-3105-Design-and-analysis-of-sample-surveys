# Code from Chapter 11 of R Companion for Sampling: Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Straight Line Regression with a Simple Random Sample ##########

##### Example 11.2

### Model-based analysis
data(anthsrs)
fit<-lm(anthsrs$height~anthsrs$finger)
summary(fit)

### Design-based analysis
anthsrs$wt<-rep(3000/200,200)
danthsrs<- svydesign(id = ~1, weight = ~wt, data = anthsrs)
degf(danthsrs)  # here, 199
fit2 <- svyglm(height~finger, design=danthsrs)  
summary(fit2)
confint(fit2)     # here calculated using normal distribution
fit2$coefficients # contains coefficients
fit2$deviance     # residual sum of squares (for this SRS example)

##### Example 11.6

### Using jackknife weights
danthsrsjk <- as.svrepdesign(danthsrs, type="JK1")
fit3 <- svyglm(height~finger, design=danthsrsjk)  
summary(fit3)
confint(fit3)


########## Linear Regression for Complex Survey Data ##########

##### Example 11.7

data(nhanes)
nhanes$ridageyr2<-nhanes$ridageyr^2
dnhanes<-svydesign(id=~sdmvpsu, strata=~sdmvstra,nest=TRUE,
                   weights=~wtmec2yr,data=nhanes)
dnhanescc <- subset(dnhanes,complete.cases(cbind(bmxbmi,ridageyr)))
fit4<-svyglm(bmxbmi~ridageyr + ridageyr2, design=dnhanescc) 
summary(fit4)
# can also extract separate elements
nobs(fit4) # number of observations used in regression
fit4$coefficients # extract regression parameters
1 - fit4$deviance/fit4$null.deviance  # R-squared
# test linear hypotheses about model terms
regTermTest(fit4,~ridageyr + ridageyr2,df=15)

### Use BRR weights
dnhanesbrr <- as.svrepdesign(dnhanes, type="BRR")
fit5<-svyglm(bmxbmi~ridageyr + ridageyr2, design=dnhanesbrr,na.action=na.omit) 
summary(fit5)

### Graph the regression equation
svyplot(bmxbmi~ridageyr, design=dnhanes, style="bubble",basecol="gray",
        inches=0.03,xlab="Age (years)",ylab="Body Mass Index",
        xlim=c(0,80),ylim=c(10,70))
# plot fitted quadratic regression line
timevalues <- seq(2, 80, 0.02)
length(timevalues)
predicted <- predict(fit4,data.frame(ridageyr=timevalues, ridageyr2=timevalues^2))
lines(timevalues, predicted, col = "black", lwd = 3)

##### Example 11.8

### Define domains and design object
nhanes$female <- nhanes$riagendr-1
nhanes$age20d<-rep(0,nrow(nhanes))
nhanes$age20d[nhanes$ridageyr >=20 & !is.na(nhanes$bmxbmi)]<-1
nhanes$bmi30<-nhanes$bmxbmi
nhanes$bmi30[nhanes$bmxbmi>30]<-1
nhanes$bmi30[nhanes$bmxbmi<=30]<-0
nhanes$raceeth <- rep(NA,nrow(nhanes))
nhanes$raceeth[nhanes$ridreth3==1 | nhanes$ridreth3==2] <- "Hispanic"
nhanes$raceeth[nhanes$ridreth3==3] <- "White"
nhanes$raceeth[nhanes$ridreth3==4] <- "Black"
nhanes$raceeth[nhanes$ridreth3==6] <- "Asian"
nhanes$raceeth[nhanes$ridreth3==7] <- "Other"
# check variable construction; display missing values
table(nhanes$age20d,nhanes$bmi30,useNA="ifany")
# no missing data for female, raceeth
table(nhanes$female,nhanes$riagendr,useNA="ifany")
table(nhanes$raceeth,nhanes$ridreth3,useNA="ifany")
dnhanes <- svydesign(id = ~sdmvpsu, strata = ~ sdmvstra, nest=TRUE,
                     weights=~wtmec2yr, data = nhanes)

### Define subset object and look at coefficient of female
dnhanescc <- subset(dnhanes,!is.na(bmxbmi))
fit6<-svyglm(bmxbmi~female, design=dnhanescc) 
summary(fit6)
2*(1-pt(2.725,15)) # calculate p-value for female with 15 df

### Look at domains as factor variable
fit7<-svyglm(bmxbmi~factor(female)-1, design=dnhanescc) 
summary(fit7)

##### Example 11.9

dnhanesadult <- subset(dnhanes,age20d==1)
fit8<-svyglm(bmxbmi~factor(raceeth), design=dnhanesadult) 
summary(fit8)
1 - fit8$deviance/fit8$null.deviance # R-squared
# test statistic for H0: all domain means are equal
regTermTest(fit8,~factor(raceeth),df=15)
# can draw side-by-side boxplot, see Figure 11.6 of SDA for plot
svyboxplot(bmxbmi~raceeth,dnhanesadult)

### Alternatively, fit model without intercept so coefficients are group means
fit9<-svyglm(bmxbmi~factor(raceeth)-1, design=dnhanesadult) 
fit9$coefficients

########## Logistic Regression ##########

##### Example 11.12

dnhanessub2<-subset(dnhanes,age20d==1 & !is.na(bmxwaist))
lrout<-svyglm(bmi30 ~ bmxwaist+ female,family=quasibinomial,
             design=dnhanessub2)
summary(lrout)
# calculate odds ratios
exp(lrout$coefficient)
# Wald test for all parameters in model
regTermTest(lrout, ~bmxwaist+female,df=15)

### Graph the predicted probabilities
waist <- seq(50,175,0.1)
xfemale <- data.frame(bmxwaist=waist,female=rep(1,length(waist)))
linfemalepred <- predict(lrout,xfemale)
xmale <- data.frame(bmxwaist=waist,female=rep(0,length(waist)))
linmalepred <- predict(lrout,xmale)
# predicted probability for female
femalepred <- exp(linfemalepred)/(1 + exp(linfemalepred))
# predicted probability for male
malepred <- exp(linmalepred)/(1 + exp(linmalepred))

par(las=1,mar=c(4,4,1,2))   
plot(waist,femalepred,type="n",xlab="Waist Circumference (cm)",
     ylab="Estimated Probability",axes=F,xlim=c(70,140))
lines(waist,femalepred,lty=1,lwd=2)
lines(waist,malepred,lty=2,lwd=2)
legend("topleft",c("Female","Male"),lty=c(1,2),bty="n")
axis(2)
axis(1)
box(bty="l")















