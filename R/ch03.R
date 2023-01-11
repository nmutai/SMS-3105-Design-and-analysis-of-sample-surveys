# Code from Chapter 3 of R Companion for Sampling: Design and Analysis by
# Yan Lu and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Allocation Methods ##########

##### Example 3.2, Compute allocations for stratified samples from agpop

data(agpop) # load the data set
names(agpop) # list the variable names
head(agpop) # take a look at the first 6 obsns
nrow(agpop) # number of rows, 3078
unique(agpop$region) # take a look at the four regions, NC, NE, S, W
table(agpop$region) # number of counties in each stratum

### Proportional allocation

popsize <- table(agpop$region)
propalloc <- 300 * popsize / sum(popsize)
propalloc

# Round to nearest integer
propalloc_int <- round(propalloc)
propalloc_int
sum(propalloc_int) # check that stratum sample sizes sum to 300


### Neyman allocation
stratvar <- c(1.1, 0.8, 1.0, 2.0)
# Make sure the stratum variances in stratvar are in same
#  order as the table in popsize
neymanalloc <- 300 * (popsize * sqrt(stratvar)) / sum(popsize * sqrt(stratvar))
neymanalloc
neymanalloc_int <- round(neymanalloc)
neymanalloc_int
sum(neymanalloc_int)

### Optimal allocation

relcost <- c(1.4, 1.0, 1.0, 1.8)
# Make sure the relative costs in relcost are in same
# order as the table in popsize
optalloc <- 300 * (popsize * sqrt(stratvar / relcost)) / sum(popsize * sqrt(stratvar / relcost))
optalloc
optalloc_int <- round(optalloc)
optalloc_int
sum(optalloc_int)

########## Selecting a Stratified Random Sample ##########

##### Example 3.2, Select a stratified sample from agpop

### Use the sample function with each stratum

# Select an SRS without replacement from each region with proportional allocation
# with total size n=300
regionname <- c("NC", "NE", "S", "W")
# Make sure sampsize has same ordering as regionname
sampsize <- c(103, 21, 135, 41)
# Set the seed for random number generation
set.seed(108742)
index <- NULL
for (i in 1:length(sampsize)) {
  index <- c(index, sample((1:N)[agpop$region == regionname[i]],
    size = sampsize[i], replace = F
  ))
}
strsample <- agpop[index, ]
# Check that we have the correct stratum sample sizes
table(strsample$region)
# Print the first six rows of the sample to see
strsample[1:6, ]

### Use the strata function from the sampling package

# Sort the population by stratum
agpop2 <- agpop[order(agpop$region), ]
# Use the strata function to select the units for the sample
# Make sure size argument has same ordering as the stratification variable
index2 <- strata(agpop2,
  stratanames = c("region"), size = c(103, 21, 135, 41),
  method = "srswor"
)
table(index2$region) # look at number of counties selected within each region
head(index2)
strsample2 <- getdata(agpop2, index2) # extract the sample
head(strsample2)

# Calculate the sampling weights
# First check that no probabilities are 0
sum(strsample2$Prob <= 0)
strsample2$sampwt <- 1 / strsample2$Prob
# Check that the sampling weights sum to the population sizes for each stratum
tapply(strsample2$sampwt, strsample2$region, sum)

########## Computing Statistics from a Stratified Random Sample ##########

##### Example 3.2 and 3.6

data(agstrat)
names(agstrat) # list the variable names
agstrat[1:6, 1:8] # take a look at the first 6 obsns from columns 1 to 8
nrow(agstrat) # number of rows, 300
unique(agstrat$region) # take a look at the four regions, NC, NE, S, W
table(agstrat$region) # number of counties in each stratum
# check that the sum of the weights equals the population size
sum(agstrat$strwt) # 3078

### Draw a boxplot of the stratified random sample

boxplot(acres92 / 10^6 ~ region,
  xlab = "Region", ylab = "Millions of Acres",
  data = agstrat
)
# notice the large variability in western region

### Set up information for the survey design

# create a variable containing population stratum sizes, for use in fpc (optional)
# popsize_recode gives popsize for each stratum
popsize_recode <- c("NC" = 1054, "NE" = 220, "S" = 1382, "W" = 422)
# next statement substitutes 1054 for each 'NC', 220 for 'NE', etc.
agstrat$popsize <- popsize_recode[agstrat$region]
table(agstrat$popsize) # check the new variable
# input design information for agstrat
dstr <- svydesign(
  id = ~1, strata = ~region, weights = ~strwt, fpc = ~popsize,
  data = agstrat
)
dstr

### Calculate the statistics using the design object

# calculate mean, SE and confidence interval
smean <- svymean(~acres92, dstr)
smean
confint(smean, level = .95, df = degf(dstr)) # note that df = n-H = 300-4
# calculate total, SE and CI
stotal <- svytotal(~acres92, dstr)
stotal
degf(dstr) # Show the degrees of freedom for the design
# calculate confidence intervals using the degrees of freedom
confint(stotal, level = .95, df = degf(dstr))

### Alternative design specifications

# Get same result if omit weights argument since weight = popsize/n_h
dstrfpc <- svydesign(id = ~1, strata = ~region, fpc = ~popsize, data = agstrat)
svymean(~acres92, dstrfpc)
# If you include weights but not fpc, get SE without fpc factor
dstrwt <- svydesign(id = ~1, strata = ~region, weights = ~strwt, data = agstrat)
svymean(~acres92, dstrwt)

### Calculating stratum means and variances

# calculate mean and se of acres92 by regions
svyby(~acres92, by = ~region, dstr, svymean, keep.var = TRUE)
# calculate total and se of acres92 by regions
svyby(~acres92, ~region, dstr, svytotal, keep.var = TRUE)

# formula calculations, using tapply
# variables sampsize and popsize were calculated earlier in the chapter
# calculate mean within each region
strmean <- tapply(agstrat$acres92, agstrat$region, mean)
strmean
# calculate variance within each region
strvar <- tapply(agstrat$acres92, agstrat$region, var)
strvar
# verify standard errors by direct formula
strse <- sqrt((1 - sampsize / popsize) * strvar / sampsize)
# same standard errors as from svyby
strse

### Estimating proportions from a stratified random sample

# Option 1: Use a 0-1 variable and find its mean
# Create variable lt200k
agstrat$lt200k <- rep(0, nrow(agstrat))
agstrat$lt200k[agstrat$acres92 < 200000] <- 1
# Rerun svydesign because the data set now has a new variable
dstr <- svydesign(
  id = ~1, strata = ~region, fpc = ~popsize,
  weights = ~strwt, data = agstrat
)
# calculate proportion, SE and confidence interval
smeanp <- svymean(~lt200k, dstr)
smeanp
confint(smeanp, level = .95, df = degf(dstr))
# calculate total, SE and CI
stotalp <- svytotal(~lt200k, dstr)
stotalp
confint(stotalp, level = .95, df = degf(dstr))

# Option 2: Create a factor variable lt200kf
agstrat$lt200kf <- factor(agstrat$lt200k)
# Rerun svydesign because the data set now has a new variable
dstr <- svydesign(
  id = ~1, strata = ~region, fpc = ~popsize,
  weights = ~strwt, data = agstrat
)
# calculate proportion, SE and confidence interval
smeanp2 <- svymean(~lt200kf, dstr)
smeanp2
confint(smeanp2, level = .95, df = degf(dstr))
# calculate total, SE and CI
stotalp2 <- svytotal(~lt200kf, dstr)
stotalp2
confint(stotalp2, level = .95, df = degf(dstr))

# Option 3: Construct asymmetric confidence intervals
# calculate proportion and confidence interval with svyciprop
svyciprop(~ I(lt200k == 1), dstr, method = "beta")
