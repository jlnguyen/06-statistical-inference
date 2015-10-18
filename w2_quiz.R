# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 2 | Quiz
#
# Joe Nguyen | 18 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# What is the variance of the distribution of the average an IID draw of n observations from a population with mean μ and variance σ2.
#
# sigma^2/n


## Question 2
# Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. About what is the probability that a random 35-44 year old has a DBP less than 70?
pnorm(70, mean = 80, sd = 10)


## Question 3
# Brain volume for adult women is normally distributed with a mean of about 1,100 cc for women with a standard deviation of 75 cc. What brain volume represents the 95th percentile?
qnorm(0.95, mean = 1100, sd = 75)


## Question 4
# Refer to the previous question. Brain volume for adult women is about 1,100 cc for women with a standard deviation of 75 cc. Consider the sample mean of 100 random adult women from this population. What is the 95th percentile of the distribution of that sample mean?
mu <- 1100
sd <- 75
p <- 0.95
n <- 100
sd_n <- sd/sqrt(n)
qnorm(p, mean = mu, sd = sd_n)

# Using simulations
nSim <- 10e3

avg_n <- apply(matrix(rnorm(nSim * n, mu, sd), nSim), 1, mean)
quantile(avg_n, p)


## Question 5
# You flip a fair coin 5 times, about what's the probability of getting 4 or 5 heads?
pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)


## Question 6
# The respiratory disturbance index (RDI), a measure of sleep disturbance, for a specific population has a mean of 15 (sleep events per hour) and a standard deviation of 10. They are not normally distributed. Give your best estimate of the probability that a sample mean RDI of 100 people is between 14 and 16 events per hour?
mu <- 15
sd <- 10
n <- 100

# CLT says XBar ~ N(mu, var/n)
pnorm(16, mu, sd/sqrt(n)) - pnorm(14, mu, sd/sqrt(n))


## Question 7
# Consider a standard uniform density. The mean for this density is .5 and the variance is 1 / 12. You sample 1,000 observations from this distribution and take the sample mean, what value would you expect it to be near?
mu <- 0.5
var <- 1/12
n <- 1e3

# CLT states Xbar = X as n -> Inf. Show with simulation
nSim <- 1e3
avg_n <- apply(matrix(runif(nSim * n), nSim), 1, mean)
mean(avg_n)


## Question 8
# The number of people showing up at a bus stop is assumed to be Poisson with a mean of 5 people per hour. You watch the bus stop for 3 hours. About what's the probability of viewing 10 or fewer people?
lambda <- 5
t <- 3
ppois(10, lambda*t)

