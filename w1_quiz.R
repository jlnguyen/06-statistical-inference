# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 1 | Quiz
#
# Joe Nguyen | 09 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Consider influenza epidemics for two parent heterosexual families. Suppose that the probability is 17% that at least one of the parents has contracted the disease. The probability that the father has contracted influenza is 12% while the probability that both the mother and father have contracted the disease is 6%. What is the probability that the mother has contracted influenza? (Hints look at lecture 2 around 5:30 and homework question on page 3/10). 
#
# A - father has influenza
# B - mother has influenza
# p(A or B) = p(A) + p(B) - p(A and B)
pAorB <- 0.17
pA <- 0.12
pAandB <- 0.06
pB <- pAorB - pA + pAandB; pB


## Question 2
# A random variable, X is uniform, a box from 0 to 1 of height 1. (So that its density is f(x)=1 for 0≤x≤1.) What is its 75th percentile? (Hints, look at lecture 2 around 21:30 and homework 1 page 4/10. Also, look up the help function for the qunif command in R.)
qunif(0.75, 0,1)
qpois(0.75, 10)

## Question 3
# You are playing a game with a friend where you flip a coin and if it comes up heads you give her X dollars and if it comes up tails she gives you Y dollars. The probability that the coin is heads is p (some number between 0 and 1.) What has to be true about X and Y to make so that both of your expected total earnings is 0. The game would then be called “fair”.
#
# (Hints, look at Lecture 4 from 0 to 6:50 and Homework 1 page 5/10. Also, for further reading on fair games and gambling, start with the Dutch Book problem ). 
#
# w(x) = E[X] = p(-X) + (1-p)Y = 0
# p / (1-p) = Y/X   q.e.d
# i.e. odds (= p/(1-p)) is equal to Y/X


## Question 4
# A density that looks like a normal density (but may or may not be exactly normal) is exactly symmetric about zero. (Symmetric means if you flip it around zero it looks the same.) What is its median?
#
# Median must be zero.


## Question 5
# What is the mean of:
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp

mu <- sum(p * x); mu


## Question 6
# A web site (www.medicine.ox.ac.uk/bandolier/band64/b64-7.html) for home pregnancy tests cites the following: “When the subjects using the test were women who collected and tested their own samples, the overall sensitivity was 75%. Specificity was also low, in the range 52% to 75%.” Assume the lower value for the specificity. Suppose a subject has a positive test and that 30% of women taking pregnancy tests are actually pregnant. What number is closest to the probability of pregnancy given the positive test?
#
# Events
# + <- positive test
# - <- negative test
# D <- has disease
# Dc <- no disease
# 
# sensitivity = p(+|D)  = p(B|A)
# specificity = p(-|Dc) = 1 - p(Bc|Ac)
#
# Bayes
# p(A|B) = p(B|A)p(A) / sum{ p(B|A)p(A), p(B|Ac)p(Ac) }
pBA <- 0.75
pBcAc <- 0.52
pA <- 0.3

num <- pBA * pA
den <- num + (1 - pBcAc) * (1 - pA)
pAB <- num / den; pAB







