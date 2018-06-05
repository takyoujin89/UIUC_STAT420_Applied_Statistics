# Practice Quiz
# 1. Consider a random variable XX that has a normal distribution with a mean of 5 and a variance of 9.
# Calculate P[x>4]
# variance = 9 => sd = 3
pnorm(4, mean = 5, sd = 3, lower.tail = FALSE)  # 0.6305587


# 2. y = -3 + 2.5 * x + epsilon(mean=0,mu2 = 4), what is E[Y|X=5]
x = 5
epsilon = rnorm(mean = 0, sd = 2)  # this is wrong, since there is no n given
# y = -3 + 2.5 * x + epsilon
y = -3 + 2.5 * x
y

# 3. SD[Y|X = 10]
# why SD[Y|X = 10] == epsilon'sd, because equal variance
# at each value of x, the variance of Y is the same, which is pu2

# 4. tree data, Fit a simple linear regression model with Girth as the response and Height as the predictor
treem = lm(Girth ~ Height, data = trees)

# 5. What is the value of R2 for this fitted SLR model?
coef(treem)  # -6.1883945 0.2557471 
summary(treem)
summary(treem)$r.squared  # 0.2696518

# Quiz
# 1. Y = 10 + 5 * x + e(mean = 0, sd = 4), calculate the probability that Y is less than 6 given that x = 0
x1 = 0
y1 = 10 + 5 * x1
y1  # y1 = 10
pnorm(6, mean = y1, sd = 4)  # 0.1586553

# 2. the probability that Y is greater than 3 given that x = -1
x2 = -1
y2 = 10 + 5 * x2
y2  # y2 = 5
pnorm(3, mean = y2, sd = 4, lower.tail = FALSE)  # 0.6914625

# 3. the probability that Y is greater than 3 given that x = -2
x3 = -2
y3 = 10 + 5 * x3
y3  # y3 = 0
pnorm(3, mean = y3, sd = 4, lower.tail = FALSE) # 0.2266274



# 4. What is the estimate of the intercept parameter?
View(faithful)

faithfulmd = lm(eruptions ~ waiting, data = faithful)
faithfulmd

# 5. What is the estimate of the intercept parameter?
coef(faithfulmd)  # (Intercept)     waiting -1.87401599  0.07562795 

# 6. predict the duration of an eruption based on a waiting time of 80 minutes.
predict(faithfulmd, newdata = data.frame(waiting = 80))  # 4.17622


# 7. predict the duration of an eruption based on a waiting time of 120 minutes. 
predict(faithfulmd, newdata = data.frame(waiting = 120))  # 7.201338 

# 8. range(faithful$waiting)  # range from 42 to 96
range(faithful$waiting)  # range from 42 to 96

# 9. the RSS for the fitted model, RSS = sum(e ^ 2)
# sum((y - y_hat) ^ 2)

# all.equal(faithful$eruptions - faithfulmd$fitted.values, faithfulmd$residuals)
sum((faithful$eruptions - faithfulmd$fitted.values)^2)  # 66.56178

# since s2_e = sum(e ^ 2) / (n - 2), and s_e = sqrt(s2_e)
# RSS = s_e ^ 2 * (n-2)
summary(faithfulmd)$sigma ^ 2 * (length(faithful$eruptions) - 2)  # 66.56178

# 10. What proportion of the variation in eruption duration is explained by the linear relationship with waiting time?
# same as r.squared
summary(faithfulmd)$r.squared  # 0.8114608

# 11. standard deviation of the residuals of the fitted model
# which is the residual standard error
summary(faithfulmd)$sigma  # 0.4965129 is not accurate

sd(summary(faithfulmd)$residuals)  # 0.495596
sd(resid(faithfulmd))

# 12. slope and intercept for least squares and maximum likelihood are same

# 13. 
# There are observations in the dataset used to fit this regression with negative yy values.
# The difference between the yy values of observations at x = 10x=10 and x = 9x=9 is 2.3.
# A good estimate for the mean of YY when x = 0x=0 is -1.5.

# C

# 14. The SLR model assumes that errors are independent.
# The SLR model assumes that the response variable follows a normal distribution.
# The SLR model allows for larger variances for larger values of the predictor variable.
# The SLR model assumes that the relationship between the response and the predictor is linear.

# AD

# 15. no relationship no

