# Coursera JHPH Data Science
# 005 - Statistical Inference
# Week 3 | Quiz
#
# Joe Nguyen | 25 Oct, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/06-inference"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# In a population of interest, a sample of 9 men yielded a sample average brain volume of 1,100cc and a standard deviation of 30cc. What is a 95% Student's T confidence interval for the mean brain volume in this new population?
n <- 9
x <- 1100
s <- 30
x + c(-1,1) * qt(0.975, n-1) * s / sqrt(n)


## Question 2
# A diet pill is given to 9 subjects over six weeks. The average difference in weight (follow up - baseline) is -2 pounds. What would the standard deviation of the difference in weight have to be for the upper endpoint of the 95% T confidence interval to touch 0?
#
# ci1 = xDiff + qt_.975 * sd / sqrt(n) <-> sd = (ci1 - xDiff) * sqrt(n) / qt_.975
n <- 9
(0 - (-2)) * sqrt(n) / qt(0.975, n-1)


## Question 3
# In an effort to improve running performance, 5 runners were either given a protein supplement or placebo. Then, after a suitable washout period, they were given the opposite treatment. Their mile times were recorded under both the treatment and placebo, yielding 10 measurements with 2 per subject. The researchers intend to use a T test and interval to investigate the treatment. Should they use a paired or independent group T test and interval?
#
# A paired interval


## Question 4
# In a study of emergency room waiting times, investigators consider a new and the standard triage systems. To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System).
#
# New system (1)
x1 <- 3
s1 <- sqrt(0.6)
n1 <- 10

# Old system (2)
x2 <- 5
s2 <- sqrt(0.68)
n2 <- 10

# Pooled variance estimate
sp <- sqrt( ( (n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2) )

# Confidence interval (independent group t CI)
x1 - x2 + c(-1,1) * qt(0.975, n1+n2-2) * sp * (1/n1 + 1/n2)^.5


## Question 5
# Suppose that you create a 95% T confidence interval. You then create a 90% interval using the same data. What can be said about the 90% interval with respect to the 95% interval?
#
# The interval will be narrower.


## Question 6
# To further test the hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment. What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis? (Because there's so many observations per group, just use the Z quantile instead of the T.) 
#
# New system (1)
x1 <- 4
s1 <- 0.5
n1 <- 100

# Old system (2)
x2 <- 6
s2 <- 2
n2 <- 100

# Pooled variance estimate
sp <- sqrt( ( (n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2) )

# Confidence interval (independent group t CI)
x2 - x1 + c(-1,1) * qnorm(0.975) * sp * (1/n1 + 1/n2)^.5
x2 - x1 + c(-1,1) * qt(0.975, n1+n2-2) * sp * (1/n1 + 1/n2)^.5


## Question 7
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the four week period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, calculate the relevant *90%* t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.
#
# Treated
x1 <- -3
s1 <- 1.5
n <- 9

# Placebo
x2 <- 1
s1 <- 1.8
n2 <- 9

# Pooled variance estimate
sp <- sqrt( ( (n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2) )

# Independent group t confidence interval 90%
x1 - x2 + c(-1,1) * qt(0.95, n1+n2-2) * sp * (1/n1 + 1/n2)^.5


