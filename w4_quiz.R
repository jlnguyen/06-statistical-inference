# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 4 | Quiz
#
# Joe Nguyen | 29 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# A pharmaceutical company is interested in testing a potential blood pressure lowering medication. Their first examination considers only subjects that received the medication at baseline then two weeks later. The data are as follows (SBP in mmHg)
# Subject 	Baseline 	Week 2
# 1 	140 	132
# 2 	138 	135
# 3 	150 	151
# 4 	148 	146
# 5 	135 	130
# Consider testing the hypothesis that there was a mean reduction in blood pressure? Give the P-value for the associated two sided T test.
# (Hint, consider that the observations are paired.)
base <- c(140, 138, 150, 148, 135)
treat <- c(132, 135, 151, 146, 130)
t.test(base, treat, paired = TRUE)$p.value


## Question 2
# A sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is the complete set of values of μ0 that a test of H0:μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?
n <- 9
mn <- 1100
s <- 30
mn + c(-1,1) * qt(0.975, n-1) * s / sqrt(n)


## Question 3
# Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people was asked which of two blinded drinks given in random order that they preferred. The data was such that 3 of the 4 people chose Coke. Assuming that this sample is representative, report a P-value for a test of the hypothesis that Coke is preferred to Pepsi using a one sided exact test.
coke <- c(1,1,1,0)
pepsi <- c(0,0,0,1)

# Hypothesis data first
t.test(coke, pepsi, paired = TRUE, alternative = "greater")$p.value
t.test(coke, pepsi, paired = TRUE, alternative = "less")$p.value
t.test(coke, pepsi, paired = TRUE, alternative = "two.sided")$p.value

# Use Binomial test
# H0: p=0.5 (equal preference for coke and pepsi)
# Ha: p>0.5 (preference for coke)
pbinom(2,4, 0.5, lower.tail = FALSE)
# ^P(X = 3 | X = 4)


## Question 4 ?? OK from discussion forum -> pval = probability of not sampling close to mean of H0. For *below* standard, look at left tail
#
# Infection rates at a hospital above 1 infection per 100 person days at risk are believed to be too high and are used as a benchmark. A hospital that had previously been above the benchmark recently had 10 infections over the last 1,787 person days at risk. About what is the one sided P-value for the relevant test of whether the hospital is *below* the standard? 
#
# Below the standard -> look at probability on left tail
ppois(10, 1787/100, lower.tail = TRUE)

# 1 - ppois(10/1787, 1/100, lower.tail = TRUE)
# 
# 1 - ppois(1/100, 1/100, lower.tail = TRUE)
# 
# 1 - ppois(1787/100, 1787/100, lower.tail = TRUE)
# 1 - ppois(10, 1787/100, lower.tail = TRUE)
# 1 - ppois(20, 1787/100, lower.tail = TRUE)


## Question 5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, give a pvalue for a two sided t test.

# group1 - pill
m1 <- -3
s1 <- 1.5
n1 <- 9

# group2 - placebo
m2 <- 1
s2 <- 1.8
n2 <- 9

# Common population variance (pooled variance) (avg since n1 == n2)
spsq <- mean(c(s1^2, s2^2))
sp <- sqrt(spsq)
se <- sp * sqrt(2/n1)
df <- n1 + n2 - 2

# t statistic
ts <- (m1 - m2) / se

# pvalue of .975 quantile
2 * pt(-abs(ts), df, lower.tail = TRUE)
# ^although should not have to do -abs() since H0 known (H0 = placebo)


## Question 6
# Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. Would you reject in a two sided 5% hypothesis test of H0:μ=1,078? 
#
# No you wouldn't reject. 1078 is inside 90% CI (1077, 1123) which puts 5% probability in each tail. So, two-sided 5% test puts 2.5% in each tail, which means CIs widen.


## Question 7
# Researchers would like to conduct a study of 100 healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the power of the study for a 5% one sided test versus a null hypothesis of no volume loss?
n <- 100
mn <- 0.01
s <- 0.04
se <- s / sqrt(n)

# Assume normal distribution (n <- 100)
pwr <- pnorm(0 + qnorm(0.95) * se, mean = mn, sd = se, lower.tail = FALSE)
pwr


## Question 8
# Researchers would like to conduct a study of n healthy adults to detect a four year mean brain volume loss of .01 mm3. Assume that the standard deviation of four year volume loss in this population is .04 mm3. About what would be the value of n needded for 90% power of type one error rate of 5% one sided test versus a null hypothesis of no volume loss?
mn <- 0.01
s <- 0.04
pwr <- 0.9
alpha <- 0.05

n <- ((qnorm(pwr) + qnorm(1 - alpha)) * s / mn)^2; ceiling(n)


## Question 9
# As you increase the type one error rate, α, what happens to power?
#
# You will get larger power.




