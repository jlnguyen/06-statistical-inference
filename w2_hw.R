# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 2 | Homework
#
# Joe Nguyen | 18 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# The probability that a manuscript gets accepted to a journal is 12% (say). However, given that a revision is asked for, the probability that it gets accepted is 90%. Is it possible that the probability that a manuscript has a revision asked for is 20%? 
#
# A <- accepted
# B <- revision
#
# P(A) <- 0.12
# P(A|B) <- 0.9
# P(B) <- 0.2?
#
# Using conditional probability, P(A|B) = P(A && B) / P(B);
# [P(A && B) subset P(A)] = P(A|B) * P(B);
# P(A): 0.12 < 0.18 (0.9 * 0.2)
# ANSWER:
# No, not possible.


## Question 2
# Suppose that the number of web hits to a particular site are approximately normally distributed with a mean of 100 hits per day and a standard deviation of 10 hits per day. What's the probability that a given day has fewer than 93 hits per day expressed as a percentage to the nearest percentage point?
#
# X <- number of hits
# x <- 93
#
# Find P(X <= x); i.e. find density (area under Normal distribution) below x == 93
# Expect P(X <= x) < 0.5 and > 0.16 (0.5 - 0.68/2)
x <- 93
mu <- 100
sd <- 10
pnorm(x, 100, 10)


## Question 3
# Suppose 5% of housing projects have issues with asbestos. The sensitivity of a test for asbestos is 93% and the specificity is 88%. What is the probability that a housing project has no asbestos given a negative test expressed as a percentage to the nearest percentage point?
#
# A <- house has asbestos
# P(A) <- 0.05
# P(+|A) <- 0.93
# P(-|!A) <- 0.88
#
# Find P(!A|-) = P(-|!A)P(!A) / ( P(-|!A)P(!A) + P(-|A)P(A) )
pA <- 0.05
pYesA <- 0.93
pNoANeg <- 0.88

pANeg <- 1 - pA
pNoA <- 1 - pYesA

num <- pNoANeg * pANeg
den <- num + pNoA * pA
pANegNo <- num / den; pANegNo


## Question 4
# Suppose that the number of web hits to a particular site are approximately normally distributed with a mean of 100 hits per day and a standard deviation of 10 hits per day.
#
# 1. What number of web hits per day represents the number so that only 5% of days have more hits? Express your answer to 3 decimal places.
#
# X <- number of hits
# Find x s.t. F(X = x) = 0.95 (CDF cumulative distribution function)
mu <- 100
sd <- 10
p <- 0.95
round(qnorm(p, mu, sd), 3)


## Question 5
# 2. Imagine taking a random sample of 50 days. What number of web hits would be the point so that only 5% of averages of 50 days of web traffic have more hits? Express your answer to 3 decimal places. 
nSim <- 10000
n <- 50

hits50dayAvg <- apply( matrix(rnorm(nSim * n, mu, sd), nSim), 1, mean)

# Get top 5% of simulations
round(quantile(hits50dayAvg, p), 3)

## Analytical solution: XBar is average hits for n = 50; then X ~ N(100, 10^2/50) ##
round(qnorm(.95, mean = 100, sd = 10 / sqrt(50) ), 3)


## Question 6
# You don't believe that your friend can discern good wine from cheap. Assuming that you're right, in a blind test where you randomize 6 paired varieties (Merlot, Chianti, ...) of cheap and expensive wines
#
# 1. What is the chance that she gets 5 or 6 right expressed as a percentage to one decimal place?
#
# SOLUTION: Binomial distribution
round(pbinom(4, size = 6, prob = 0.5, lower.tail = FALSE) * 100, 1)


## Question 7
# Consider a uniform distribution. If we were to sample 100 draws from a uniform distribution (which has mean 0.5, and variance 1/12) and take their mean, Xˉ
#
# 1. What is the approximate probability of getting as large as 0.51 or larger expressed to 3 decimal places?
#
# SOLUTION: Use CLT (central limit theorem) which says XBar ~ N(mu, var/n)
mu <- 0.5
var <- 1/12
n <- 100
round(pnorm(0.51, mu, sqrt(var/n), lower.tail = FALSE), 3)


## Question 8
# If you roll ten standard dice, take their average, then repeat this process over and over and construct a histogram,
#
# 1. what would it be centered at?
#
# SOLUTION: dice roll has uniform distribution
nSim <- 10000
n <- 10

avg_n <- apply(matrix(runif(nSim * n, min = 1, max = 6), nSim), 1, mean)
hist(avg_n)
mean(avg_n)


## Question 9
# 2. what would be its variance expressed to 3 decimal places?
round(var(avg_n), 3)

# Analytical Solution:
min <- 1
max <- 6
varDice <- ((max - min + 1)^2 - 1) / 12
var_n <- round(varDice / n, 3); var_n

# Variance of single dice roll using definition: var = E[(X - E[X])^2]
varDice2 <- mean((1:6 - mean(1:6))^2); varDice2


## Question 10
# The number of web hits to a site is Poisson with mean 16.5 per day. What is the probability of getting 20 or fewer in 2 days expressed as a percentage to one decimal place?
lambda <- 16.5
t <- 2
round(ppois(20, lambda*t) * 100, 1)





