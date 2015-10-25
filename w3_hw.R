# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 3 | Homework
#
# Joe Nguyen | 25 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Load the data set mtcars in the datasets R package. Calculate a 95% confidence interval to the nearest MPG for the variable mpg.
data("mtcars")
head(mtcars)
str(mtcars)
n <- dim(mtcars); n <- n[1]

mean(mtcars$mpg) + c(-1,1) * qt(0.975, n-1) * sd(mtcars$mpg) / sqrt(n)

# or
t.test(mtcars$mpg)$conf.int


## Question 2
# Suppose that standard deviation of 9 paired differences is 1. What value would the average difference have to be so that the lower endpoint of a 95% students t confidence interval touches zero?
#
# CI_0 = X_bar - TQ * SE_Est;
# X_bar = CI_0 + TQ * SE_Est;
n <- 9
round(0 + qt(0.975, n-1) * 1 / sqrt(n), 2)


## Question 3
# An independent group Student's T interval is used instead of a paired T interval when:
# B) The observations between the groups are naturally assumed to be statistically independent.
#
# Answer notes:
# We can't pair them if the groups are independent of each other as well as independent within themselves.


## Question 4
# Consider the mtcars dataset. Construct a 95% T interval for MPG comparing 4 to 6 cylinder cars (subtracting in the order of 4 - 6) assume a constant variance.
mtcarsCyl46 <- subset(mtcars, cyl %in% c(4, 6))
head(mtcarsCyl46)
t.test(mpg ~ cyl, paired = FALSE, var.equal = TRUE, data = mtcarsCyl46)$conf.int

#or
m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
#this does 4 - 6
as.vector(t.test(m4, m6, var.equal = TRUE)$conf.int)


## Question 5
# If someone put a gun to your head and said "Your confidence interval must contain what it's estimating or I'll pull the trigger", what would be the smart thing to do?
#
# Make your interval as wide as possible


## Question 6
# Refer back to comparing MPG for 4 versus 6 cylinders. What do you conclude?
#
# The interval is above zero, suggesting 4 is better than 6 in the terms of MPG


## Question 7
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects' body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was 3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. The study aims to answer whether the change in BMI over the four week period appear to differ between the treated and placebo groups.
#
# What is the pooled variance estimate? (to 2 decimal places)
treat.mean <- 3
treat.sd <- 1.5
treat.n <- 9

base.mean <- 1
base.sd <- 1.8
base.n <- 9

# t interval for unequal variances
base.var <- base.sd^2 / base.n
treat.var <- treat.sd^2 / treat.n
tdf <- (base.var + treat.var)^2 / ( base.var/(base.n-1) + treat.var/(treat.n-1) )
mnse <- (base.var + treat.var)^.5

round((treat.mean - base.mean) + c(-1,1) * tdf * mnse, 2)

# pooled variance estimate (since n equal for both groups)
sp2 <- (base.sd^2 + treat.sd^2) / 2; round(sp2, 2)

## or
n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
spsq <- ( (n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)


## Question 8
# For Binomial data the maximum likelihood estimate for the probability of a success is:
#
# The proportion of successes.


## Question 9
# Bayesian inference requires:
#
# Assigning a prior probability distribution.





