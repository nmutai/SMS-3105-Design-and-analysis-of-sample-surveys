# Code from Chapter 4 of R Companion for Sampling: Design and Analysis by 
# Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Ratio Estimation ##########

##### Examples 4.2 and 4.3

data(agsrs)
n<-nrow(agsrs) #300
agsrs$sampwt <- rep(3078/n,n)
agdsrs <- svydesign(id = ~1, weights=~sampwt, fpc=rep(3078,300), data = agsrs)
agdsrs
# correlatIon of acres87 and acres92
cor(agsrs$acres87,agsrs$acres92) 
# estimate the ratio acres92/acres87
sratio<-svyratio(numerator = ~acres92, denominator = ~acres87,design = agdsrs)
sratio
confint(sratio, df=degf(agdsrs))

# provide the population total of x
xpoptotal <- 964470625
# Ratio estimate of population total
predict(sratio,total=xpoptotal)
# Ratio estimate of population mean
predict(sratio,total=xpoptotal/3078)

# draw the scatterplot
par(las=1) # make tick mark labels horizontal (optional)
plot(x=agsrs$acres87/1e6,y=agsrs$acres92/1e6, 
  xlab="Millions of Acres Devoted to Farms (1987)",
  ylab = "Millions of Acres Devoted to Farms (1992)",
  main = "Acres Devoted to Farms in 1987 and 1992")
# draw line through origin with slope Bhat
abline(0,coef(sratio))

##### Example 4.5

# scatterplot and correlation of seed92 and seed94
data(santacruz)
plot(santacruz$seed92,santacruz$seed94,
     main="Number of seedlings in 1994 and 1992",
     xlab="Number of seedlings in 1992",ylab="Number of seedlings in 1994")
cor(santacruz$seed92,santacruz$seed94)

nrow(santacruz) #10
santacruz$sampwt <- rep(1,nrow(santacruz)) 
design0405 <- svydesign(ids = ~1, weights = ~sampwt, data = santacruz)
design0405
#Ratio estimation using number of seedlings of 1992 as auxiliary variable
sratio3<-svyratio(~seed94, ~seed92,design = design0405)
sratio3
confint(sratio3, df=10-1)

########## Regression Estimation ##########

##### Example 4.7

data(deadtrees)
head(deadtrees)
nrow(deadtrees) # 25
# Fit with survey regression
dtree<- svydesign(id = ~1, weight=rep(4,25), fpc=rep(100,25), data = deadtrees)
myfit1 <- svyglm(field~photo, design=dtree) 
summary(myfit1) # displays regression coefficients
confint(myfit1,df=23) # df = 25-2
# Regression estimate of population mean field trees
newdata <- data.frame(photo=11.3)
predict(myfit1, newdata)
confint(predict(myfit1, newdata),df=23)
# Estimate total field tree, add population size in total= argument
newdata2 <- data.frame(photo=1130)
predict(myfit1, newdata2, total=100)  
confint(predict(myfit1, newdata2,total=100),df=23)

########## Domain Estimation ##########

##### Example 4.8

agsrsnew<-agsrs #copy agsrs as agsrsnew, since we want to create a new column 
# we calculated sampwt in the first code in this chapter
# define new variable farmcat
agsrsnew$farmcat<-rep("large",n)
agsrsnew$farmcat[agsrsnew$farms92 < 600] <- "small"
head(agsrsnew)
dsrsnew <- svydesign(id = ~1, weights=~sampwt, fpc=rep(3078,300), data=agsrsnew)  
# domain estimation for large farmcat with subset statement
dsub1<-subset(dsrsnew,farmcat=='large')   # design info for domain large farmcat
smean1<-svymean(~acres92,design=dsub1)   
smean1
df1<-sum(agsrsnew$farmcat=='large')-1 #calculate domain df if desired
df1
confint(smean1, level=.95,df=df1) # CI
stotal1<-svytotal(~acres92,design=dsub1)
stotal1
confint(stotal1, level=.95,df=df1)
# domain estimation for small farmcat 
dsub2<-subset(dsrsnew,farmcat=='small')  # design info for domain small farmcat         
smean2<-svymean(~acres92,design=dsub2)   
smean2
df2<-sum(agsrsnew$farmcat=='small')-1 #calculate domain df if desired
confint(smean2, level=.95,df=df2) #CI
stotal2<-svytotal(~acres92,design=dsub2)
stotal2
confint(stotal2, level=.95,df=df2)

# use svyby function
bothtot<-svyby(~acres92,by=~factor(farmcat),design=dsrsnew,svytotal)
bothtot
confint(bothtot,level=.95)
bothmeans<-svyby(~acres92,by=~factor(farmcat),design=dsrsnew,svymean)
bothmeans
confint(bothmeans,level=.95)

########## Poststratification ##########

##### Example 4.9

data(agsrs)
dsrs <- svydesign(id = ~1, weights=rep(3078/300,300), fpc=rep(3078,300), 
                  data = agsrs) 
# Create a data frame that gives the population totals for the poststrata
pop.region <- data.frame(region=c("NC","NE","S","W"), Freq=c(1054,220,1382,422))
# create design information with poststratification
dsrsp<-postStratify(dsrs, ~region, pop.region)
summary(dsrsp)
1/unique(dsrsp$prob)  # See the poststratified weight for each region
svymean(~acres92, dsrsp)
svytotal(~acres92, dsrsp)


########## Ratio Estimation with Stratified Sampling ##########

##### Combined ratio estimator

data(agstrat)
popsize_recode <- c('NC' = 1054, 'NE' = 220, 'S' = 1382, 'W' = 422)
agstrat$popsize <- popsize_recode[agstrat$region]
# input design information for agstrat
dstr <- svydesign(id = ~1, strata = ~region, fpc = ~popsize, weight = ~strwt, 
                  data = agstrat) 
# now compute the combined estimator of the ratio
combined<-svyratio(~ acres92,~acres87,design = dstr)
combined
# we can get the combined ratio estimator of the population total 
# with the predict function
predict(combined,total=964470625)

##### Separate ratio estimator

separate<-svyratio(~acres92,~acres87,design = dstr,separate=TRUE)
separate
#  Define the stratum totals for acres87 as a list:
stratum.xtotals <- list(NC=350474227,NE=22033421,S=280631939,W=311331038)
predict(separate,stratum.xtotals)

########## Model-Based Ratio and Regression Estimation ##########

##### Example 4.11

data(agsrs)
# define weights to use for weighted least squares analysis
agsrs$recacr87<-agsrs$acres87
agsrs$recacr87[agsrs$acres87!=0] <- 1/agsrs$acres87[agsrs$acres87!=0]
agsrs$recacr87[agsrs$acres87==0] <- NA
# fit weighted least squares model without intercept
fit<-lm(acres92~acres87-1,weights=recacr87,data=agsrs)
summary(fit)
anova(fit)
# find predicted value at population total for x
newdata3 <- data.frame(acres87=964470625)
predict(fit, newdata3, se.fit=TRUE)  

# plot weighted residual versus predicted values
wresid<-fit$residuals*sqrt(fit$weights)
par(las=1)
plot(fit$fitted.values, wresid, 
     main="Plot of weighted residuals versus predicted values",
     xlab="Predicted value from regression model",
     ylab="Weighted residuals")  

##### Example 4.12

data(deadtrees)
# Fit with lm
fit2 <- lm(field~photo, data=deadtrees) 
summary(fit2)
# Estimate mean field trees
newdata <- data.frame(photo=11.3)
predict(fit2, newdata,se.fit=TRUE)

# plot residuals versus predicted values
plot(deadtrees$photo, fit2$residuals, 
     main="Plot of residuals versus photo values",
     xlab="Photo values (x variable)",
     ylab="Residuals")







