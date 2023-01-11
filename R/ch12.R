# Code from Chapter 12 of R Companion for Sampling: Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Two-Phase Sampling ##########

##### Examples 12.1 and 12.4

data(vietnam)
print.data.frame(vietnam[1:6,])
nrow(vietnam)  #2064
# define logical variable to specify which obsns are selected in phase 2
vietnam$indexp2<- vietnam$p2sample==1
dphase2<-twophase(id=list(~1,~1), weights=list(~phase1wt, ~phase2wt),
        strata=list(NULL,~apc), subset=~indexp2, data=vietnam, method="simple")
svymean(~vietnam, dphase2)

########## Estimating the Size of a Population ##########

##### Example 13.1

# create data frame of records from sample 2 of size n2
n1 <- 200
n2 <- 100
m <- 20
fish<-data.frame(x=c(rep(1,m),rep(0,n2-m)),wt=rep(1,n2),n1=rep(n1,n2))
dfish<-svydesign(id=~1,weights=~wt,data=fish)
estpop<-svyratio(~n1,~x,dfish)
estpop
### Calculate symmetric confidence interval
confint(estpop,df=n2-1)

### Invert hypothesis test
# define xmat and y
xmat <- cbind(c(1,1,0),c(1,0,1))
y <- c(20,180,80)
cbind(xmat,y) # show xmat and y
captureci(xmat,y)

### Use bootstrap
chapman<-function(y,n1) { (n1+1)*(length(y)+1)/(sum(y)+1) - 1}
Ntilde<-chapman(fish[,1],n1)
Ntilde
# generate 2000 bootstrap samples
nboot<-2000
set.seed(9231)
bootsamp<-matrix(sample(fish[,1], size = nrow(fish)*nboot, replace=TRUE), ncol=nboot)
dim(bootsamp) # nboot columns of resamples
# calculate Chapman's estimate for each column
Ntildeboot<-apply(bootsamp,2,chapman,n1)
# draw histogram of bootstrap distribution
par(las=1)
hist(Ntildeboot,xlab = "Estimated Population Size", 
  main = "Histogram of Bootstrap Estimates",col="lightgray",
  breaks=20,border="white")
box(bty="l")
# calculate percentiles to get confidence interval
quantile(Ntildeboot,probs=c(0.025,0.975))

##### Example 13.3

# define xmat2 and y2
xmat2<-cbind(c(1,1,1,0,1,0,0),c(1,1,0,1,0,1,0),c(1,0,1,1,0,0,1))
y2 <- c(6,27,314,8,1728,69,712)
# apply captureci
captureci(xmat2,y2)
















