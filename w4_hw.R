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
# 

