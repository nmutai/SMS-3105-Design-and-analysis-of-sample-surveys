# Code from Chapter 6 of R Companion for Sampling: 
# Design and Analysis by Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Selecting a Sample with Unequal Probabilities ##########

##### Example 6.2

data(classes)
classes[1:2,]
N<-nrow(classes)
set.seed(78065)
# select 5 classes with probability proportional to class size and with replacement
sample_units<-sample(1:N,5,replace=TRUE,prob=classes$class_size)
sample_units
mysample<-classes[sample_units,]  
mysample  
# calculate ExpectedHits and sampling weights
mysample$ExpectedHits<-5*mysample$class_size/sum(classes$class_size)
mysample$SamplingWeight<-1/mysample$ExpectedHits
mysample$psuid<-row.names(mysample)
mysample
# check sum of sampling weights
sum(mysample$SamplingWeight)  

# sampling without replacement
set.seed(330582)
cluster(data=classes, clustername=c("class"), size=5, method="systematic",
        pik=classes$class_size,description=TRUE)

########## Selecting a Two-Stage Cluster Sample ##########

##### Example 6.11

# create data frame classeslong
data(classes)
classeslong<-classes[rep(1:nrow(classes),times=classes$class_size),]
classeslong$studentid <- sequence(classes$class_size) 
nrow(classeslong)
table(classeslong$class) # check class sizes
head(classeslong)

# select a two-stage cluster sample, psu: class, ssu: studentid 
# number of psus selected: n = 5 (pps systematic)
# number of students selected: m_i = 4 (srs without replacement)
# problist<-list(classes$class_size/647) # same results as next command
problist<-list(classes$class_size/647,4/classeslong$class_size) #selection prob
problist[[1]]  # extract the first object in the list. This is pps, size M_i/M
problist[[2]][1:5] # first 5 values in second object in list
# number of psus and ssus
n<-5
numberselect<-list(n,rep(4,n))
numberselect
# two-stage sampling
set.seed(75745)
tempid<-mstage(classeslong,stage=list("cluster","stratified"), 
               varnames=list("class","studentid"),
               size=numberselect, method=list("systematic","srswor"),pik=problist)

# get data
sample1<-getdata(classeslong,tempid)[[1]]
# sample 1 contains the ssus of the 5 psus chosen at the first stage
# Prob_ 1 _stage has the first-stage selection probabilities
head(sample1) 
nrow(sample1)
table(sample1$class) # lists the psus selected in the first stage
sample2<-getdata(classeslong,tempid)[[2]]
# sample 2 contains the final sample
# Prob_ 2 _stage has the second-stage selection probabilities
# Prob has the final selection probabilities
head(sample2)  
nrow(sample2) # sample of 20 ssus altogether
table(sample2$class) # 4 ssus selected from each psu
# calculate final weight = 1/Prob
sample2$finalweight<-1/sample2$Prob
# check that sum of final sampling weights equals population size
sum(sample2$finalweight)
sample2[,c(1,2,3,6,7)] # print variables from final sample

### use Sampford's method

# select a cluster sample in two stages, psu: class, ssu: studentid 
# number of psu selected n =5 (Sampford's method)
# first, convert the measure of size to a vector of probabilities
classes$stage1prob<-inclusionprobabilities(classes$class_size,5)
sum(classes$stage1prob)  # inclusion probabilities sum to n
# select the psus
set.seed(29385739)
stage1.units<-UPsampford(classes$stage1prob)
stage1.sample<-getdata(classes,stage1.units)
stage1.sample

# first-stage units are in stage1.sample
# now select the second-stage units (students)
# convert the psus in the sample to long format and assign student ids
npsu<-nrow(stage1.sample)
stage1.long<-stage1.sample[rep(1:npsu,times=stage1.sample$class_size),]
stage1.long$studentid<-sequence(stage1.sample$class_size) 
head(stage1.long)
# use strata function to select 4 ssus from each psu
stage2.units<-strata(stage1.long, stratanames=c("class"), 
                     size=rep(4,5), method="srswor")
nrow(stage2.units)
# get the data for the second-stage sample
ssusample<-getdata(stage1.long,stage2.units)
head(ssusample)

# compute the sampling weights
# stage1prob contains stage 1 sampling probability; 
# Prob has stage 2 sampling probability
ssusample$finalprob<- ssusample$stage1prob*ssusample$Prob
ssusample$finalwt<-1/ssusample$finalprob
sum(ssusample$finalwt)  # check sum of weights
# print selected columns of ssusample
print(ssusample[,c(1,2,3,4,6,8,9)],digits=4)

########## Computing Estimates from an Unequal-Probability Sample ##########

##### Example 6.4

studystat <- data.frame(class = c(12, 141, 142, 5, 1),
                        Mi = c(24, 100, 100, 76, 44), 
                        tothours=c(75,203,203,191,168))
studystat$wt<-647/(studystat$Mi*5)
sum(studystat$wt) # check weight sum, which estimates N=15 psus
# design for with-replacement sample, no fpc argument
d0604 <- svydesign(id = ~1, weights=~wt, data = studystat)
d0604
# Ratio estimation using Mi as auxiliary variable
ratio0604<-svyratio(~tothours, ~Mi,design = d0604)
ratio0604
confint(ratio0604, level=.95,df=4)
# Can also estimate total hours studied for all students in population
svytotal(~tothours,d0604)

##### Example 6.6

students <- data.frame(class = rep(studystat$class,each=5),
   popMi = rep(studystat$Mi,each=5), 
   sampmi=rep(5,25),
   hours=c(2,3,2.5,3,1.5,2.5,2,3,0,0.5,3,0.5,1.5,2,3,1,2.5,3,5,2.5,4,4.5,3,2,5))
# The 'with' function allows us to calculate using variables from a data frame
# without having to type the data frame name for all of them
students$studentwt <- with(students,(647/(popMi*5)) * (popMi/sampmi))
# check the sum of the weights
sum(students$studentwt)
# create the design object
d0606 <- svydesign(id = ~class, weights=~studentwt, data = students)
d0606
# estimate mean and SE
svymean(~hours,d0606)
degf(d0606) 
confint(svymean(~hours,d0606),level=.95,df=4) #use t-approximation

# estimate total and SE
svytotal(~hours,d0606)
confint(svytotal(~hours,d0606),level=.95,df=4)

##### Example 6.11

data(classpps)
nrow(classpps)
head(classpps)
d0611 <- svydesign(ids = ~class, weights=~classpps$finalweight, data = classpps)
d0611
# estimate mean and SE
svymean(~hours,d0611)
confint(svymean(~hours,d0611),level=.95,df=4) #use t-approximation
# estimate total and SE
svytotal(~hours,d0611)
confint(svytotal(~hours,d0611),level=.95,df=4)

##### Example 6.8

supermarket<-data.frame(store=c('A','B','C','D'),area=c(100,200,300,1000),
                        ti=c(11,20,24,245))
supermarket

supermarket$psi<-supermarket$area/sum(supermarket$area)
psii<-supermarket$area/sum(supermarket$area)
piik<- psii %*% t(psii/(1-psii)) + (psii/(1-psii)) %*% t(psii)
diag(piik)<-rep(0,4) # set the diagonal entries of the matrix equal to zero
piik  # joint inclusion probabilities
pii<-apply(piik,2,sum)
pii # inclusion probabilities

##### Example 6.9

supermarket2<-supermarket[3:4,]
supermarket2$pii <- pii[3:4] # these are the unit inclusion probs when n=2
jointprob<-piik[3:4,3:4]  # joint probability matrix for stores C and D
diag(jointprob)<-supermarket2$pii # set diagonal entries equal to pii
jointprob
# Horvitz-Thompson type 
dht<- svydesign(id=~1,  fpc=~pii, data=supermarket2, 
                   pps=ppsmat(jointprob),variance="HT")  
dht
svytotal(~ti,dht)
# Sen-Yates-Grundy type
dsyg<- svydesign(id=~1,  fpc=~pii, data=supermarket2, 
                   pps=ppsmat(jointprob),variance="YG")  
dsyg
svytotal(~ti,dsyg)

##### Example 6.10

data(agpps)
jtprobag<-as.matrix(agpps[,20:34])
diag(jtprobag)<-agpps$SelectionProb
# Horvitz-Thompson type 
dhtag<- svydesign(id=~1,  fpc=~SelectionProb, data=agpps, 
                   pps=ppsmat(jtprobag),variance="HT")  
svytotal(~acres92,dhtag)
# Sen-Yates-Grundy type
dsygag<- svydesign(id=~1,  fpc=~SelectionProb, data=agpps, 
                   pps=ppsmat(jtprobag),variance="YG")  
svytotal(~acres92,dsygag)
# Hartley-Rao approximation
sumsqprob<-sum(agpps$SelectionProb^2)/nrow(agpps)
dHRag<-svydesign(id=~1, fpc=~SelectionProb, data=agpps, 
                   pps=HR(sumsqprob),variance="YG")  
svytotal(~acres92,dHRag)
# With-replacement variance
dwrag<-svydesign(id=~1, weights=~SamplingWeight, data=agpps)
svytotal(~acres92,dwrag)