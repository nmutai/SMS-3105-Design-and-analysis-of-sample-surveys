# Code from Chapter 2 of R Companion for Sampling: Design and Analysis by Yan Lu 
# and Sharon L. Lohr
# All code is presented for educational purposes only and without warranty.

##### Install the R packages needed for the chapter

library(survey)
library(sampling)
library(SDAResources)

########## Selecting a Simple Random Sample ##########

##### Example 2.5

# Set the seed for random number generation
set.seed(108742)
# Select an SRS of size n=4 from a population of size N=10 without replacement
srs4 <- sample(1:10, 4, replace = FALSE)
srs4
# Print the sample to see
# Can print an object by typing its name, or typing "print(object)"
print(srs4)
srs4

# Run again, without setting a new seed.
sample(1:10, 4, replace = FALSE)
# Now go back to original seed.
set.seed(108742)
sample(1:10, 4, replace = FALSE)

# Using the sample function to select an SRS with replacement
set.seed(101)
srswr4 <- sample(1:10, 4, replace = TRUE)
srswr4

# Using function srswor from the sampling package

set.seed(1329)
# Select an SRS of size n=4 from a population of size N=10 without replacement.
s1 <- srswor(4, 10)
# List the units in the sample (the population units having s1=1).
s1
(1:10)[s1 == 1]


# Select an SRS of size n=4 from a population of size N=10 with replacement.
set.seed(35882)
s2 <- srswr(4, 10)
# the selected units are 2 and 9
s2
(1:10)[s2 != 0]
# number of replicates, units 2 and 9 both appear twice
s2[s2 != 0]
# can use the getdata function to extract the sample from data frame with population
popdf <- data.frame(popid = 1:10)
getdata(popdf, s2)

# Call a function using variable names
set.seed(35882)
srswr(4, 10)
set.seed(35882)
srswr(n = 4, N = 10)
set.seed(35882)
srswr(N = 10, n = 4)

##### Example 2.6, select the sample

# Select a different sample of size 300 from agpop
# Load the SDAResources package containing all the data in SDA book
# library(SDAResources)  # we comment this since we already loaded the package
data(agpop) # Load the data set agpop
N <- nrow(agpop)
N # 3078 observations
# Select an SRS of size n=300 from agpop
set.seed(8126834)
index <- srswor(300, N)
# each unit k is associated with index 1 or 0, with 1 indicating selection
index[1:10]
# agsrs2 is an SRS with size 300 selected from agpop
# extract the sampled units from the data frame containing the population
agsrs2 <- getdata(agpop, index)
nrow(agsrs2) # 300
agsrs2 <- agpop[(1:N)[index == 1], ] # alternative way to extract the sampled units
head(agsrs2)

# Create the variable of sampling weights
n <- nrow(agsrs2)
agsrs2$sampwt <- rep(3078 / n, n)
# Check that the weights sum to N
sum(agsrs2$sampwt)

########## Computing Statistics from a Simple Random Sample ##########

##### Examples 2.6, 2.7, and 2.11

# Draw a histogram
hist(agsrs$acres92,
  breaks = 20, col = "gray", xlab = "Acres devoted to farms, 1992",
  main = "Histogram: Number of acres devoted to farms, 1992")

# Base R functions such as t.test will calculate statistics for an SRS,
# but without the fpc.
t.test(agsrs$acres92)

# Calculate the statistics by direct formula
n <- length(agsrs$acres92)
ybar <- mean(agsrs$acres92)
ybar
hatvybar <- (1 - n / 3078) * var(agsrs$acres92) / n
hatvybar

seybar <- sqrt(hatvybar)
seybar

# Calculate confidence interval by direct formula using t distribution
Mean_CI <- c(ybar - qt(.975, n - 1) * seybar, ybar + qt(.975, n - 1) * seybar)
names(Mean_CI) <- c("lower", "upper")
Mean_CI

# To obtain estimates for the population total,
# multiply each of ybar, seybar, and Mean_CI by N = 3078
seybar * 3078
Mean_CI * 3078
# Calculate coefficient of variation of mean
seybar / ybar

### Using functions in the survey package

# Create the variable of sampling weights
n <- nrow(agsrs)
agsrs$sampwt <- rep(3078 / n, n)
# Create variable lt200k
agsrs$lt200k <- rep(0, n)
agsrs$lt200k[agsrs$acres92 < 200000] <- 1
# look at the first 10 observations with column 3 (acres92) and column 17 (lt200k)
agsrs[1:10, c(3, 17)]

# Specify the survey design.
# This is an SRS, so the only design features needed are the weights
#    or information used to calculate the fpc.
dsrs <- svydesign(id = ~1, weights = ~sampwt, fpc = rep(3078, 300), data = agsrs)
dsrs

# Calculate the mean for acres92 and its standard error using the svymean function.
smean <- svymean(~acres92, dsrs)
smean
# Use the confint function to compute a 95% confidence interval from
# the information in smean, df = n-1 = 300-1 = 299
confint(smean, df = 299)
# Repeat these steps with the svytotal function to obtain estimated totals.
stotal <- svytotal(~acres92, dsrs)
stotal
confint(stotal, df = 299)
# Calculate the CV of the mean
SE(smean) / coef(smean)
# or
smean <- as.data.frame(smean)
smean[[2]] / smean[[1]]

# Estimate population means for multiple variables
agsrs_means <- svymean(~ acres92 + lt200k, dsrs)
agsrs_means
confint(agsrs_means, df = 299)

# Analyzing a categorical variable that is coded as characters
# First, display the category names and counts
table(agsrs$region)
# Find the estimated proportions in each category
region_prop <- svymean(~region, dsrs)
region_prop
confint(region_prop, df = 299)
region_total <- svytotal(~region, dsrs)
region_total
confint(region_total, df = 299)

# Analyzing a categorical variable that is coded as numbers
# First, analyze lt200k as a numeric variable (works only if all values are 0 or 1)
# This gives the mean of variable lt200k, which is the proportion with lt200k = 1.
svymean(~lt200k, dsrs)
# Now, analyze lt200k as a factor variable. This gives the proportion
# in each category.
svymean(~ factor(lt200k), dsrs)

##### Exercise 2.27 of SDA

# Calculating bootstrap means for Exercise 2.27 in SDA
set.seed(244)
B <- 1000
n <- length(agsrs$acres92)
boot.samples <- matrix(sample(agsrs$acres92, size = B * n, replace = TRUE), B, n)
boot.statistics <- apply(boot.samples, 1, mean)
hist(boot.statistics,
  main = "Estimated Sampling Distribution of ybar",
  xlab = "Mean of acres92 from Bootstrap Replicate",
  col = "gray", border = "white")
