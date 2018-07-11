# Practice Quiz
# 1. Consider a random variable X that has an F distribution with 33 and 55 degrees of freedom
# Calcualte P[x>2.7]
pf(2.7 , df1 = 3, df2 = 5, lower.tail = FALSE)  # 0.1561342

#2. or Questions 2-5, use the built-in longley dataset in R. Fit a multiple linear regression model with Employed as the response. 
# Use three predictors: GNP, Population, and Armed.Forces.
# Create a 90% confidence interval for beta_1 Report the lower bound of this interval.
View(longley)
longley$Armed.Forces
longley_model = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
coef(longley_model)
confint(longley_model, level = 0.9, parm = "GNP")
# 0.05579357 0.1040607

# 3. What is the standard error of beta_2_hat?
summary(longley_model)
summary(longley_model)$coef["Population", "Std. Error"] #  0.1859156

# 4. What is the p-value for testing H0: beta_3 = 0 vs H1: beta_3 != 0
summary(longley_model)$coef["Armed.Forces", "Pr(>|t|)"]
# 0.0970466

# 5. What is the value of the FF test statistic for testing for significance of regression?
names(summary(longley_model))
# [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"      
# [6] "sigma"         "df"            "r.squared"     "adj.r.squared" "fstatistic"   
# [11] "cov.unscaled"
summary(longley_model)$fstatistic  # 238.5757

# Quiz
# 1. Consider testing for significance of regression in a multiple linear regression model with 9 predictors and 30 observations
# if F = 2.4, what is p-value?
# (p-1) 9 predictors => p coefficients p = 9+1 = 10
# n = 30, p = 10, df1 = p-1 = 9, df2 = n-p = 20
1 - pf(2.4, df1 = 9, df2 = 20)  #  0.04943057


# t value for a null hypothesis , the distribution for this follows that of a t distribution( mean = 0, df depends on n and p)
# A t distribution is very close to a normal distribution, and the more df, the closer it is to a normal one 
# A t distribution is symmetric , so when you are calculating the probability of seeing an observation corresponding to a specific quantile value (Pr > |t|) , you have to multiply it by two.
# A F distribution is right skewed and not symmetric . df1= p-1, df2 = n-p 

# 2. What is the p-value for testing H0: beta_1 = 0 vs H1 beta_1 != 0 in a multiple linear regression model 
# with 5 predictors and 20 observations if the value of the t test statistic is -1.3?

# no need to calculate f
# n = 20, p = 5 + 1 = 6
# df = n - p = 14
pt(-1.3, df = 14) * 2  # 0.2145976

pf((-1.3)^2, df1 = 6, df2 = 14, lower.tail = FALSE) # wrong
1 - pf((-1.3)^2, df1 = 6, df2 = 14) # wrong

# 3.SD[beta_2_hat] =?
# beta_2_hat ~ N(beta_2, sigma ^ 2 * C22)
set.seed(42)

  x1 = runif(15)
  x2 = runif(15)
  x3 = runif(15)
x1
x2
x3
x_values
beta_0 = 3
beta_1 = 2
beta_2 = 0.5
beta_3 = 5

sigma = 3
x0 = rep(1, 15)
X = cbind(x0, x1, x2, x3)
C = solve(t(X) %*% X)
sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])  #  2.47399

# 4. For Questions 4-11, use the swiss dataset, which is built into R.
?swiss
sw_model = lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)
summary(sw_model)
new_province = data.frame(Agriculture = 54, Examination = 23, Education = 13, 
                          Catholic = 60,  Infant.Mortality = 24)
new_province
predict(sw_model, newdata = new_province, interval = "prediction")
# fit 72.46069

# 5. Create a 99% confidence interval for the coefficient for Catholic. Report the upper bound of this interval.
confint(sw_model, parm = "Catholic", level = 0.99)
# Catholic 0.008877479 0.1993532
0.1993532


# 6. Calculate the p-value of the test H0: beta_examinaiton = 0 vs H1 !=0
summary(sw_model)
summary(sw_model)$coef["Examination", "Pr(>|t|)"]  # 0.3154617

# 7. Create a 95% confidence interval for the average Fertility for a Swiss province in 1888 with:
new_province2 = data.frame(Agriculture = 40, Examination = 28, Education = 10, 
                          Catholic = 42,  Infant.Mortality = 27)
predict(sw_model, newdata = new_province2, interval = "confidence", level = 0.95)
# fit     lwr      upr
# 1 77.55014 69.4446 85.65567
# 69.4446


# 8.Create a 95% prediction interval for the Fertility of a Swiss province in 1888 with:
new_province2 = data.frame(Agriculture = 40, Examination = 28, Education = 10, 
                           Catholic = 42,  Infant.Mortality = 27)
predict(sw_model, newdata = new_province2, interval = "prediction", level = 0.95)
# fit      lwr      upr
# 1 77.55014 60.96392 94.13635
# 60.96392

# 9. Report the value of the FF statistic for the significance of regression test.
summary(sw_model)$fstatistic  # 19.76106

# 10. alpha = 0.01, decision: reject H0

# 11. Consider a model that only uses the predictors Education, Catholic, and Infant.Mortality. 
# Use an F test to compare this with the model that uses all predictors. 
# Report the p-value of this test.

sw_model_2 = lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss)
summary(sw_model_2)$fstatistic
anova(sw_model_2, sw_model)
# 0.05628 


# 12.  Consider two nested multiple linear regression models fit to the same data.
# R1 ^ 2 = 0.9, R2 ^ 2 0.8 Which model uses fewer predictors?
# 0.8

# 13. The following multiple linear regression is fit to data
# Y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
# if beta_1_hat = 5, beta_2_hat = 0.25 the
# not enough

# 14. Suppose you have an SLR model for predicting IQ from height. 
# The estimated coefficient for height is positive. 
# Now, we add a predictor for age to create an MLR model. 
# After fitting this new model, the estimated coefficient for height must be:
# none

# 15. If the FF test for the significance of regression has a p-value less than 0.01, then we know that:
# 1 is wrong
# 2 correct: both can be greater than 0.01
