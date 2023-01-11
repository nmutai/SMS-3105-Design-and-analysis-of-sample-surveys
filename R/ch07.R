# Code from Chapter 7 of R Companion for Sampling: Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Selecting a Stratified Two-Stage Sample ##########

# create data frame classeslong  (from ch06.R)
data(classes)
classeslong<-classes[rep(1:nrow(classes),times=classes$class_size),]
classeslong$studentid <- sequence(classes$class_size) 
nrow(classeslong)
table(classeslong$class) # check class sizes
head(classeslong)
# define strata
classeslong$strat<-rep(3,nrow(classeslong))
classeslong$strat[classeslong$class_size > 40]<-2
classeslong$strat[classeslong$class_size > 70]<-1
# table(classeslong$class,classeslong$strat)
# order data by stratum
classeslong2<-classeslong[order(classeslong$strat),]
# check the stratum construction
table(classeslong2$strat,classeslong2$class_size)
nrow(classeslong2) # number of students in population

# select a stratified two stage cluster sample, 
# stratum: strat
# psu: class 
# ssu: studentid 
# number of psus selected n =2, size=rep(n=2,3 strata) (srswor)
# number of students selected m_i =3 size=rep(m_i= 3,6 classes) (srswor)
numberselect<-list(table(classeslong2$strat),rep(2,3),rep(3,6))
numberselect
# select a stratified two-stage cluster sample
set.seed(75745)
tempid<-mstage(classeslong2,stage=list("stratified","cluster","stratified"), 
               varnames=list("strat","class","studentid"),
               size=numberselect, method=list("","srswor","srswor"))
# get data
sample3<-getdata(classeslong2,tempid)[[3]]  #3rd stage
sample3$finalweight<-1/sample3$Prob
# check sum of weights, should be close to number of students in population
# (but not exactly equal, since psus not selected with prob proportional to M_i)
sum(sample3$finalweight) 
sample3 # print the sample

sample1<-getdata(classeslong2,tempid)[[1]]  #1st stage
sample2<-getdata(classeslong2,tempid)[[2]]  #2nd stage
names(sample1)
table(sample1$`Prob_ 1 _stage`)
table(sample2$strat,sample2$`Prob_ 2 _stage`) # Selection probs for psus in strata
table(sample3$class,sample3$`Prob_ 3 _stage`) # Selection probs for ssus in psus


########## Estimating Quantiles ##########

##### Example 7.5

data(htsrs)
dhtsrs<-svydesign(id = ~1,weights=rep(2000/200,200),fpc=rep(2000,200), data=htsrs)
# cdf treated as step function, gives values in Table 7.1 of SDA
svyquantile(~height, dhtsrs, quantiles=c(0.25,0.5,0.75,0.9), ties = "discrete")
# interpolated quantiles (usually preferred method)
svyquantile(~height, dhtsrs, quantiles=c(0.25,0.5,0.75,0.9), ties = "rounded")

##### Examples 7.6 and 9.12

data(htstrat)
popsize_recode <- c('F' = 1000, 'M' = 1000)
# create a new variable popsize for population size
htstrat$popsize<-popsize_recode[htstrat$gender]
head(as.data.frame(htstrat))
# design object
# svydesign calculates the weights here from the fpc argument
dhtstrat<-svydesign(id = ~1, strata = ~gender, fpc = ~popsize,
          data = htstrat) 
# ties = "discrete" gives values in Table 7.1 of SDA
svyquantile(~height, dhtstrat, c(0.25,0.5,0.75,0.9), ties = "discrete")
# ties = "rounded" gives values in Example 9.12 of SDA
svyquantile(~height, dhtstrat, c(0.25,0.5,0.75,0.9), ties = "rounded",
            ci=TRUE, interval.type = "Wald")

########## Computing Estimates from Stratified Multistage Samples ##########

##### Example 7.9

data(nhanes)
nrow(nhanes) #9971
names(nhanes)
# count number of observations with missing value for ridageyr, bmxbmi
sum(is.na(nhanes$ridageyr)) # ridageyr gives age in years
sum(is.na(nhanes$bmxbmi))   # bmxbmi gives BMI
# define age20d and bmi30
nhanes$age20d<-rep(0,nrow(nhanes))
nhanes$age20d[nhanes$ridageyr >=20 & !is.na(nhanes$bmxbmi)]<-1
nhanes$bmi30<-nhanes$bmxbmi
nhanes$bmi30[nhanes$bmxbmi>30]<-1
nhanes$bmi30[nhanes$bmxbmi<=30]<-0
nhanes$bmi30<-factor(nhanes$bmi30) # set bmi30 as a categorical variable
# check missing value counts for new variables
sum(is.na(nhanes$age20d))
sum(is.na(nhanes$bmi30))
sum(nhanes$age20d) # how many records in domain?
head(nhanes)

# stratified cluster design 
d0709 <- svydesign(id = ~sdmvpsu, strata=~sdmvstra, weights=~wtmec2yr,
                   nest=TRUE, data = nhanes)
# domain estimation, age20+ 
d0709sub<-subset(d0709, age20d ==1) 
d0709sub

# Request means and design effects
nhmeans<-svymean(~bmxbmi+bmi30, d0709sub, deff=TRUE)
degf(d0709sub)
nhmeans
confint(nhmeans,df=degf(d0709sub))
# Find quantiles
svyquantile(~bmxbmi, d0709sub, quantiles=c(0.05,0.25,0.5,0.75,0.95), 
            ties = "rounded",ci=TRUE, interval.type="Wald")

########## Univariate Plots from Complex Surveys ##########

##### Examples 7.10, 7.11, and 7.12

# histogram and smoothed density function
data(htstrat)
# set graphics parameters, 1*2 plots, axis labels horizontal
par(mfrow=c(1,2),las=1,mar=c(2.1,4.1,2.1,0.3)) 
# Histogram overlaid with kernel density curve (without weight information)
# Displays the sample values, but does not estimate population histogram
# freq=FALSE changes the vertical axis to density
# breaks tell how many breakpoints to use
hist(htstrat$height,main="Without weights", xlab = "Height (cm)",
     breaks = 10, col="gray90", freq=FALSE, xlim=c(140,200), ylim=c(0,0.045))
# overlaid with kernel density curve
lines(density(htstrat$height),lty=1,lwd=2)

# Histogram (with weight information)
# create survey design object, weights calculated from fpc here
d0710 <- svydesign(id = ~1, strata = ~gender, fpc = c(rep(1000,160),rep(1000,40)),
                data = htstrat) 
d0710
svyhist(~height,d0710, main="With weights",xlab = "Height (cm)",
        breaks = 10, col="gray90", freq=FALSE,xlim=c(140,200), ylim=c(0,0.045))
dens1<-svysmooth(~height,d0710,bandwidth=5)
lines(dens1,lwd=2) # draw the density line

# boxplot
par(mfrow=c(1,2),las=1,mar=c(2.1,4.1,2.1,0.3)) 
# boxplot (with weight information)
svyboxplot(height~1,d0710,ylab="Height",xlab=" ", main="Full sample")
svyboxplot(height~gender,d0710,ylab="Height",xlab="Gender", 
           main="Separately by gender")

# histograms for domains
# Restore graphics settings
par(mfrow=c(1,1),las=1,mar=c(5.1, 4.1, 4.1, 2.1))
svyhist(~bmxbmi,d0709sub, main="Histogram of body mass index for adults",
        breaks = 30, col="gray90",xlab = "Body Mass Index (kg/m^2)")
dens2<-svysmooth(~bmxbmi,d0709sub)
lines(dens2,lwd=2)

########## Scatterplots from Complex Surveys ##########

### Unweighted plots
# scatterplot without weights
par(las=1) # make tick mark labels horizontal
plot(nhanes$ridageyr,nhanes$bmxbmi,xlab="Age (years)",ylab="Body Mass Index",
     main="Scatterplot without weights",pch=3,cex=0.5,
     ylim=c(10,70),xlim=c(0,80))

### Plot subsample of data
# select subsample with probability proportional to weights
set.seed(2847654)
subsamp<-sample(1:nrow(nhanes),500,replace=TRUE,prob=nhanes$wtmec2yr)
par(las=1) # make tick mark labels horizontal
plot(nhanes$ridageyr[subsamp],nhanes$bmxbmi[subsamp],
     xlab="Age (years)",ylab="Body Mass Index",
     main="Scatterplot of pps subsample",pch=3,cex=0.5,
     ylim=c(10,70),xlim=c(0,80))

### Bubble plots
par(las=1) # make tick mark labels horizontal
svyplot(bmxbmi~ridageyr, design=d0709, style="bubble", inches=0.03,
        xlab="Age(years)",ylab="Body Mass Index",xlim=c(0,80),ylim=c(10,70),
        main="Weighted bubble plot of BMI versus age")

### Plot data for a domain
# define subset
d0709subA<-subset(d0709, ridreth3==6)  
par(las=1) # make tick mark labels horizontal
svyplot(bmxbmi~ridageyr, design=d0709subA, style="bubble",inches = 0.03,
        xlab="Age(years)",ylab="Body Mass Index",xlim=c(0,80),ylim=c(10,70),
        main="Weighted bubble plot of BMI versus age for Asian Americans")

### Side-by-side boxplots
# include agegroup in the data frame
nhanes$agegroup<-5*round(nhanes$ridageyr/5)
d0709 <- svydesign(id = ~sdmvpsu, strata = ~ sdmvstra, nest=TRUE, 
                   weights=~wtmec2yr, data = nhanes)
par(las=1) # make tick mark labels horizontal
svyboxplot(bmxbmi~factor(agegroup),d0709,ylab="Body mass index",xlab="Age Group",
           ylim=c(10,70),main="Side-by-side boxplots of BMI for age groups")

### Smoothed trend line for mean
# Smoothed trend line with bubble plot of BMI versus age
# plot data bmxbmi~ridageyr
par(las=1) # make tick mark labels horizontal
svyplot(bmxbmi~ridageyr, design=d0709, style="bubble",basecol="gray",inches=0.03,
        xlab="Age(years)",ylab="Body Mass Index",xlim=c(0,80),ylim=c(10,70),
        main="Smoothed trend line with bubble plot of BMI versus age")
# plot smoothing trend line
# library(KernSmooth)  # install and load the package if not already done
smth<-svysmooth(bmxbmi~ridageyr,d0709)
lines(smth,lwd=2)

### Smoothed trend lines for quantiles
# Smoothed quantile trend lines with bubble plot of BMI versus age
# library(quantreg)) # install and load the package if not already done
# plot data bmxbmi~ridageyr
par(las=1) # make tick mark labels horizontal
svyplot(bmxbmi~ridageyr, design=d0709, style="bubble",basecol="gray",inches=0.03,
        xlab="Age (years)",ylab="Body Mass Index",xlim=c(0,80),ylim=c(10,70),
        main="Smoothed quantile trend lines")
# plot smoothed trend lines for quantiles
taus<-c(.05,.25,.5,.75,.95)
for (i in 1:length(taus)) {
 qsmth<-svysmooth(bmxbmi~ridageyr,d0709, quantile=taus[i],method="quantreg")
 lines(qsmth,lwd=1.2)
}


########## Empirical Probability Mass and Distribution Functions ##########

##### Example 7.5

# Empirical pmf for stratified sample of heights
# define sampling weight
htstrat$sampwt <- 1000/sum(htstrat$gender=="F")
htstrat$sampwt[htstrat$gender=="M"] <- 1000/sum(htstrat$gender=="M")
# use function emppmf to calculate pmf
strresult <- emppmf(htstrat$height,htstrat$sampwt)
# plot 
par(las=1)
plot(strresult$vals, strresult$epmf,type="h",xlab="Height Value, y (cm)", 
     ylab="Empirical pdf",lwd=1.2,
     main="Empirical pdf for stratified sample of heights (weighted)")

# Empirical cdf of height for data htpop, and for data htstrat, with and without weights
# data(htstrat)
# Recall that 
# d0710 <- svydesign(id = ~1, strata = ~gender, fpc = c(rep(1000,160),rep(1000,40)),
#                   data = htstrat) 
cdf.weighted<-svycdf(~height, d0710)
cdf.weighted
## evaluate the function for height 144
cdf.weighted[[1]](144)
## compare to population and unweighted sample ecdfs.
cdf.pop<-ecdf(htpop$height)    # ecdf for population
cdf.samp<-ecdf(htstrat$height) # unweighted ecdf of sample
par(las=1,mar=c(5.1,4.1,2.1,2.1))
plot(cdf.pop, do.points = FALSE, 
     xlab="Height value y",ylab="Empirical cdf",xlim=c(135,205),lwd=2.5,
     main="Empirical cdfs for population and sample")
lines(cdf.samp, col="red", do.points = FALSE, lwd=1.8)
lines(cdf.weighted[[1]], do.points = FALSE,  col ="purple",lwd=1)
legend("topleft", legend=c("Population", "Sample unweighted", "Sample weighted"),
       col=c("black", "red", "purple"),lwd=c(2.5,1.8,1),cex=0.8,bty="n")

########## ADDITIONAL CODE NOT IN BOOK ##########

########## Graphing Complex Survey Data with ggplot2 Functions ##########

### Install the ggplot2 package if you have not already done so

library(ggplot2)
data(nhanes)

# scatterplot without weights
plot1 <- ggplot(nhanes, aes(x=ridageyr, y=bmxbmi)) + geom_point()
# print plot with title and axis labels
plot1 + labs(title = "Unweighted scatterplot of BMI versus age", x = "Age (years)", y = "Body mass index")

# ggplot scatterplot weighted
# initial plot without weights
plot2 <- ggplot(nhanes, aes(x = ridageyr, y = bmxbmi))+ geom_point()
# adding title, x, and y labels
plot2 <- plot2 + labs(title="Bubble plot, size proportional to sum of observation weight",
            x ="Age (years)", y = "Body mass index")
# find the sum of weights at each distinct (x,y) pair using stat_sum
g2 <- plot2 + stat_sum(aes(weight = wtmec2yr, size = ..n..))  
print(g2)

# binned circle plot
mround <- function(x,base){
  base*round(x/base)
}
nhanes$bmigroup<-mround(nhanes$bmxbmi,5)
nhanes$agegroup<-mround(nhanes$ridageyr,5)
circleage<-nhanes[order(nhanes$agegroup),]
plot3 <- ggplot(circleage, aes(x = agegroup, y = bmigroup)) + labs(title="Binned circle plot",
            x ="Age (years)", y = "Body mass index")
g3 <- plot3 + stat_sum(aes(weight = wtmec2yr, size = ..n..)) 
print(g3)

# side-by-side boxplots
plot4 <- ggplot(circleage, aes(x = factor(agegroup), y = bmxbmi)) 
plot4 <- plot4 + stat_sum(aes(weight=wtmec2yr, size = ..n..)) +
         labs(title="Boxplots", x ="Age (years)", y = "Body mass index")
g4 <- plot4 + stat_summary(fun=mean, geom="point", shape=23, size=4) +geom_boxplot()
print(g4)







