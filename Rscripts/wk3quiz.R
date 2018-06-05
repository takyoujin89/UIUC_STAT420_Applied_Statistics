# prep
# t-stat = 2.3, df = 25
# one-sided p-value, or p(t>2.3)
pt(q = 2.3, df = 25, lower.tail = FALSE)  # 0.01503675

# two-sided p-value
pt(q = 2.3, df = 25, lower.tail = FALSE) + pt(q = -2.3, df = 25, lower.tail = TRUE)
pt(q = 2.3, df = 25, lower.tail = FALSE) * 2

# find t for 95% confidence
# value of t with 2.5% in each tail
qt(p = 0.025, df = 25, lower.tail = TRUE)


# Practice Quiz
# 1. Consider a random variable X that has a t distribution with 7 degrees of freedom
# Calculate P[X > 1.3]
?pt
pt(q = 1.3, df = 7, lower.tail = FALSE)  # 0.1173839

# 2. Consider a random variable Y that has a t distribution with 9 degrees of freedom. 
# Find c such that P[X > c] = 0.025.
qt(0.025, df = 9)
qt(0.025, df = 9, lower.tail = FALSE)  #  2.262157

# 3. For this exercise, use the built-in trees dataset in R. 
# Fit a simple linear regression model with Girth as the response and Height as the predictor. 
# What is the p-value for testing H0: beta_1 = 0 vs H1: beta_1 != 0
tree_md = lm(Girth ~ Height, data = trees)

?trees
names(trees)
summary(tree_md)
summary(tree_md)$coefficients[2, 4]  # 0.002757815

# 4.What is the length of a 90% confidence interval for beta_1 
confint(tree_md, parm = "Height", level = 0.9)
0.1229463 - 0.388548

# 0.2656018

# 5. Continue using the SLR model you fit in Question 3.
# Calculate a 95% confidence interval for the mean tree girth of a tree that is 79 feet tall.
new_h = data.frame(Height = 79)
predict(tree_md, newdata = new_h,
        interval = c("confidence"), level = 0.95)
# 15.12646
 
# Use lower.tail=TRUE if you are, e.g., finding the probability at the 
# lower tail of a confidence interval or if you want to the probability 
# of values no larger than z.
# 
# Use lower.tail=FALSE if you are, e.g., trying to calculate test value 
# significance or at the upper confidence limit, or you want the 
# probability of values z or larger.
# 
# You should use pnorm(z, lower.tail=FALSE) instead of 1-pnorm(z) 
# because the former returns a more accurate answer for large z.
  
# Quiz
# 1. Consider a random variable X that has a t distribution with 5 degrees of freedom. 
# Calculate P[|X| > 2.1].
pt(q = 2.1, df = 5, lower.tail = FALSE)  #  0.04487662
# note the |x|, two-sided
pt(q = 2.1, df = 5, lower.tail = FALSE) + pt(q = -2.1, df = 5, lower.tail = TRUE) 
# 0.08975325


# 2. Calculate the critical value used for a 90% confidence interval about the slope parameter 
# of a simple linear regression model that is fit to 10 observations. 
# (Your answer should be a positive value.)
qt(p = 0.05, df = 8, lower.tail = FALSE)  #  1.859548


# 3. beta_0 = 5, beta_1 = 4, sigma = 2, Sxx = 1.5
# calcualte P[beta_1_hat > 4.2]
# wrong pt(q = 4,2, df = 20 - 2, lower.tail = FALSE)  # 0.05156777
# note that The standard deviation of your beta distribution is not the standard deviation of the Error. 
sigma = 2
Sxx = 1.5
sd = sqrt(sigma ^ 2 / Sxx)
sd
pnorm(4.2, mean = 4, sd = sd, lower.tail = FALSE)  # 0.4512616
  

# 4. faithful dataset: fit a simple linear model in R that accomplishes this task.
# SE[beta_1_hat]
names(faithful)
faithfulmd
summary(faithfulmd)$coefficients[2, 2] # 0.002218541

# 5. What is the value of the test statistic for testing H0: beta_0 = 0
summary(faithfulmd)$coefficients[1,4]  # 7.359171e-26
summary(faithfulmd)$coefficients[1,3]  # -11.70212

# 6. What is the value of the test statistic for testing H0: beta_1 = 0
# p-value
summary(faithfulmd)$coefficients[2,4]  # 8.129959e-100
# t-value
summary(faithfulmd)$coefficients[2,3]  # 34.08904 
# 7. reject H0

# 8. Calculate a 90% confidence interval for beta_0. 
# Report the upper bound of this interval
confint(faithfulmd, parm = "(Intercept)", level = 0.9)
# -2.138335 -1.609697

# 9. Calculate a 95% confidence interval for beta_1. 
# Report the length of the margin of this interval.
# length of the margin = crit * beta_1_se
1 - (1-0.95) / 2
crit = qt(0.975, df = nrow(faithful) - 2)
crit
beta_1_se = summary(faithfulmd)$coef[2,2]
beta_1_se
# wrong crit * beta_1_hat_se  # 0.818057

# margin length = (upper - lower)/2
confint(faithfulmd, parm = "waiting", level = 0.95)
(0.07126011 - 0.07999579) / 2  # 0.00436784



# 10. Create a 90% confidence interval for the mean eruption duration for a waiting time of 81 minutes. 
# Report the lower bound of this interval. # 4.189899
new_w = data.frame(waiting = c(81, 72))
predict(faithfulmd, newdata = new_w,
        interval = c("confidence"), level = 0.9)
# fit      lwr      upr
# 1 4.251848 4.189899 4.313797
# 2 3.571196 3.521343 3.621050


# 11. Create a 99% prediction interval for a new observation's eruption duration for a waiting time of 72 minutes. 
# Report the upper bound of this interval. # 4.861612
# note interval = c("prediction)
predict(faithfulmd, newdata = new_w,
        interval = c("prediction"), level = 0.99)
# fit      lwr      upr
#1 4.251848 2.960139 5.543557
#2 3.571196 2.280781 4.861612

# 12. Consider a 90% confidence interval for the mean response and a 90% prediction interval, 
# both at the same xx value. Which interval is narrower? CI

# 13. Suppose you obtain a 99% confidence interval for beta_1 (-0.4, 5.2)
# H0: beta_1 = 0 what's your decision?
# 0 is in the range, fail to reject

# 14. Suppose you test H0: beta_1 = 0 with alpha = 0.01. which is true:
# The probability of observing the estimated value of beta_1 or 
# something more extreme) is greater than 0.01 if we assume that beta_1 = 0

# 15. Consider a 95% confidence interval for the mean response calculated at x = 6. 
# If instead we calculate the interval at x = 7, select each value that would change
# Estimate
# SE
