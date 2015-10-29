# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 4 | Homework
#
# Joe Nguyen | 27 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Load the data set mtcars in the datasets R package. Assume that the data set mtcars is a random sample. Compute the mean MPG, xˉ, of this sample.

# You want to test whether the true MPG is μ0 or smaller using a one sided 5% level test. (H0:μ=μ0 versus Ha:μ<μ0). Using that data set and a Z test:
    
# Based on the mean MPG of the sample xˉ, and by using a Z test: what is the smallest value of μ0 that you would reject for (to two decimal places)?

data("mtcars")
mpg <- mtcars$mpg
n <- length(mpg)
xBar <- mean(mpg)
se <- sd(mpg) / sqrt(n)

# xBar = mu0 + zq * se;
# mu0 = xBar - zq * se
mu0 <- xBar - qnorm(0.05) * se; round(mu0,2)


## Question 2
# Consider again the mtcars dataset. Use a two group t-test to test the hypothesis that the 4 and 6 cyl cars have the same mpg. Use a two sided test with unequal variances.

# 1. Do you reject at the 5% level (enter 0 for no, 1 for yes).
# 2. What is the P-value to 4 decimal places expressed as a proportion?


# mpgCyl <- subset(mtcars, cyl == 4 | cyl == 6, c(mpg, cyl))

library(dplyr)
mpgCyl <- mtcars %>% filter(cyl == 4 | cyl == 6) %>%
    select(mpg, cyl)

# # Wrong
# testDat <- t.test(mpgCyl$mpg, mpgCyl$cyl, paired = TRUE, var.equal = FALSE, alternative = "two.sided")

# Right
m4 <- filter(mpgCyl, cyl == 4) %>% select(mpg)
m6 <- filter(mpgCyl, cyl == 6) %>% select(mpg)
testDat <- t.test(m4, m6, paired = FALSE, var.equal = FALSE, alternative = "two.sided")

round(testDat$p.value, 4)
testDat


## Question 3
# A sample of 100 men yielded an average PSA level of 3.0 with a sd of 1.1. What are the complete set of values that a 5% two sided Z test of H0:μ=μ0 would fail to reject the null hypothesis for? Enter lower and upper values (2dp)
round( 3 + c(-1,1) * qnorm(0.975) * 1.1 / sqrt(100), 2 )
round( c(qnorm(0.025, mean = 3, sd = 1.1 / sqrt(100)), qnorm(0.975, mean = 3, sd = 1.1 / sqrt(100))), 2 )


## Question 4
# You believe the coin that you're flipping is biased towards heads. You get 55 heads out of 100 flips.
# 1. What's the exact relevant pvalue to 4 decimal places (expressed as a proportion)?
# 2. Would you reject a 1 sided hypothesis at αlpha=.05? (0 for no 1 for yes)?
pbinom(54, 100, 0.5, lower.tail = FALSE)

# Can't reject null (coin is fair) since p-value = 0.1841 > 0.05


## Question 5
# A web site was monitored for a year and it received 520 hits per day. In the first 30 days in the next year, the site received 15,800 hits. Assuming that web hits are Poisson.
#
# Give an exact one sided P-value to the hypothesis that web hits are up this year over last to four significant digits (expressed as a proportion).
#
# Does the one sided test reject (0 for no 1 for yes)?
lambda <- 520
t <- 30

ppois(15800 - 1, lambda * t, lower.tail = FALSE)
1 - ppois(15800, lambda * t, lower.tail = TRUE)

# Can't reject; marginally since 0.05533 > 0.05


## Question 6
# Suppose that in an AB test, one advertising scheme led to an average of 10 purchases per day for a sample of 100 days, while the other led to 11 purchaces per day, also for a sample of 100 days. Assuming a common standard deviation of 4 purchases per day. Assuming that the groups are independent and that the days are iid, perform a Z test of equivalence. 
#
# 1. What is the P-value reported to 3 digits expressed as a proportion?
# 2. Do you reject the test? (0 for no 1 for yes).

m1 <- 10
m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1/n1 + 1/n2)

# Z statistic (quantile value of normal distribution)
zs <- (m1 - m2) / se

pval <- 2 * (1 - pnorm(abs(zs)))
pval <- 2 * pnorm(-abs(zs)); round(pval, 3)

# Can't reject as 0.077 > 0.05


## Question 7
# A confidence interval for the mean contains:
# All of the values of the hypothesized mean for which we would fail to reject with α=1−Conf.Level.


## Question 8
# Consider two problems previous. Assuming that 10 purchases per day is a benchmark null value, that days are iid and that the standard deviation is 4 purchases for day. Suppose that you plan on sampling 100 days. What would be the power for a one sided 5% Z mean test that purchases per day have increased under the alternative of μ=11 purchase per day?
alpha <- 0.05
z <- qnorm(1 - alpha)
se <- s/sqrt(n1)
pwr <- pnorm(m1 + z * se, mean = m2, sd = se, lower.tail = FALSE)
round(pwr, 3)


## Question 9
# Researchers would like to conduct a study of healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3.
#
# What is necessary sample size for the study for a 5% one sided test versus a null hypothesis of no volume loss to achieve 80% power? (Always round up)
mn <- 0.01
s <- 0.04
pwr <- 0.8

# Z >= mn / (s/sqrt(n)) + z_.95;
# Z = qnorm(1 - pwr)
# (Z - z_.95) >= mn * sqrt(n) / s
# sqrt(n) <= (Z - z_.95) * s / mn
# n <= ((Z - z_.95) * s / mn)^2

n <- ((qnorm(pwr) + qnorm(0.95)) * s / mn)^2; ceiling(n)
# ??


# Question 10
# In a court of law, all things being equal, if via policy you require a lower standard of evidence to convict people then: 
# More innocent people will be convicted.


## Question 11
# Consider the mtcars data set.
# 1. Give the p-value for a t-test comparing MPG for 6 and 8 cylinder cars assuming equal variance, as a proportion to 3 decimal places.
mpg6 <- filter(mtcars, cyl == 6) %>% select(mpg); mpg6 <- mpg6[[1]]
mpg8 <- filter(mtcars, cyl == 8) %>% select(mpg); mpg8 <- mpg8[[1]]

t.test(mpg6, mpg8, paired = FALSE, var.equal = TRUE, alternative = "two.sided")$p.value

# 2. Give the associated P-value for a z test.
m6 <- mean(mpg6); s6 <- sd(mpg6); n6 <- length(mpg6)
m8 <- mean(mpg8); s8 <- sd(mpg8); n8 <- length(mpg8)


## Actually, ok now, equation in Hypothesis testing chapter missing t_df; equations for equal and unequal are equivalent to ones in 

# # NOT SURE WHY CAN'T USE THIS FORMULATION (FROM HYPOTHESIS TESTING CHAPTER) (OR WHY DIFFERENT TO CORRECT ONE (FROM T CONFIDENCE INTERVALS CHAPTER))
# se <- sqrt(s6^2/n6 + s8^2/n8)
# z <- (m6 - m8) * se

# CORRECT SOLUTION:
# Use "independent group" t confidence intervals. Groups are not paired. Also, we have different sample sizes.
#
# Assuming constant variance across the groups, we can use the "pooled variance estimator" which is a weighted average of the group-specific variances (more weight to group with more samples)
spsq <- ((n6 - 1) * s6^2 + (n8 - 1) * s8^2) / (n6 + n8 - 2)
se <- sqrt(spsq * (1/n6 + 1/n8))
z <- (m6 - m8) * se

pz <- 2 * pnorm(-abs(z)); pz

# 3. Give the common standard deviation estimate for MPG across 6 and 8 cylinders to 3 decimal places.
round(sqrt(spsq), 3)


## Question 11
# The Bonferonni correction controls this:
# The familywise error rate
