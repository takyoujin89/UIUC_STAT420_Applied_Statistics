# Read the question, carefully! WHICH MODEL


mpg[mpg$hwy > 35, c("manufactor", "model", "year")]
subset(mpg, subset = hwy > 35, select = c("manufactor", "model", "year"))


# To calculate the value of the pdf at x = 3, that is, the height of the curve at x = 3, use:
dnorm(x = 3, mean = 2, sd = 5)
## [1] 0.07820854

#To calculate the value of the cdf at x = 3, that is,  P(X <= 3), the probability that X is less than or equal to 3, use:
pnorm(q = 3, mean = 2, sd = 5)
## [1] 0.5792597

# To calculate the quantile for probability 0.975, use:
qnorm(p = 0.975, mean = 2, sd = 5)
## [1] 11.79982

# to generate a random sample of size n = 10, use:
rnorm(n = 10, mean = 2, sd = 5)


mean(subset(airquality, Month == 5)$Wind)

mean(airquality$Ozone, na.rm = TRUE)

# Residual Standard Error
summary(stop_dist_model)$sigma
## [1] 15.37959

predict(stop_dist_model, newdata = cars) 
# same as 
predict(stop_dist_model)
# same as
fitted(stop_dist_model)

n = nrow(faithful)
n
# calcualte RSS same as 
sum(resid(faithful_md) ^ 2)
(summary(faithful_md)$sigma) ^ 2 * (n-2)

#beta_1 follow N(beta_1_hat, sigma ^ 2 * (1 / n + mean(x) ^ 2/ Sxx)
#beta_0 follow N(beta_0_hat, sigma ^ 2 / Sxx)
Sxx = 1.5
beta_1_sigma = sqrt(4 / Sxx)
pnorm(q = 4.2, mean = 4, sd = beta_1_sigma, lower.tail = FALSE)

# calculate P[|x| > 2.1]
# multifly 2, since ||
pt(q = 2.1, df = 5, lower.tail = FALSE) * 2
0.04487662
0.08975325

#length of the margin of this interval
interval = confint(faithful_md, level = 0.95)[2, 2] - confint(faithful_md, level = 0.95)[2, 1]
margin = interval / 2
margin
0.00436784

# anova
faithful_null = lm(eruptions ~ 1, data = faithful)
anova(faithful_null, faithful_md)
# df2 = n - p, Error DF
anova(faithful_null, faithful_md)[2, "Res.Df"]
# df1 = p - 1, Regression DF
anova(faithful_null, faithful_md)[2, "Df"]

#  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    271 353.04                                  
#2    270  66.56  1    286.48 1162.1 < 2.2e-16 ***


set.seed(42)
x_values = data.frame(
  x0 = rep(1, 15),
  x1 = runif(15),
  x2 = runif(15),
  x3 = runif(15))


sigma = 3
beta_0 = 3
beta_1 = 2
beta_2 = 0.5
beta_3 = 5


x0 = rep(1, 15)
x1 = runif(15)
x2 = runif(15)
x3 = runif(15)
X
X = cbind(x0, x1, x2, x3)
# or X = as.matrix(cbind(rep(1, 15), x_values))
C = solve(t(X) %*% X)
# calculate SD[beta_2_hat]
# no need for y, just need to calculate C
# don't forget to sqrt
sqrt(sigma ^ 2 * C[3, 3])
6.120625

#quiz4 13, 14

#midterm
#After setting the given seed, generate 350 random observations from a normal distribution with a mean of 3 and a variance of 11. What proportion of these observations are larger than 4? (Your answer should be a number between 0 and 1.)
set.seed(42)
l = rnorm(n = 350, mean = 3, sd = sqrt(11))
mean(l > 4)
[1] 0.3514286

# week7
# factor: dummy variable choose reference level alphabetaically, beta_0 => 0

#Continue to use the model from Exercise 7. Create a 90% confidence interval for the difference in mean sepal length between virginicas and setosas for a given petal length. Report the lower bound of this interval

confint(iris_add, parm = "Speciesvirginica", level = 0.90)[, "5 %"]


# week8
# call plot with model argument, residuals vs fitted plot and qq plot will be shown
plot(mpg_hp_add)

sqrt(mean((initech$salary - fitted(initech_fit)) ^ 2))
# when use log(y) transformation, calcualte residual, transform back to the data scale by exp()
sqrt(mean((initech$salary - exp(fitted(initech_fit_log))) ^ 2))

fit_2 = lm(y ~ poly(x, 2), data = data_higher)
fit_4 = lm(y ~ poly(x, 4), data = data_higher)

# same 
fit_4b = lm(y ~ poly(x, 4, raw = TRUE), data = data_higher)
fit_4c = lm(y ~ x + I(x ^ 2) + I(x ^ 3) + I(x ^ 4), data = data_higher)
 
# Calculate the residual sum of squares (RSS) in the original units of y 
sum((y - exp(fitted(quiz_log))) ^ 2) / 1000000000  # wrong, specify y from which data !!!
sum((quiz_data$y - exp(fitted(quiz_log))) ^ 2) / 1000000000

# week9
library(leaps)
all_hipcenter_mod = summary(regsubsets(hipcenter ~ ., data = seatpos)

all_hipcenter_mod$which

all_hipcenter_mod$rss

#!!! remember to specify n, or you can't use bic
n = length(hipcenter_md)
hipcenter_mod_back_bic = step(hipcenter_md, direction = "backward", k = log(n))

# log(mpg) ~ . ^ 2 will automatically consider all first-order terms as well as all two-way interactions(only interaction, no poly)
# We use I(var_name ^ 2) to add quadratic terms for some variables
autompg_int = lm(mpg ~ (cyl + disp + hp + wt + acc + year + domestic) ^ 2, data = autompg)
autompg_big_mod = lm(log(mpg) ~ . ^ 2 + I(disp ^ 2) + I(hp ^ 2) + I(wt ^ 2) + I(acc ^ 2), data = autompg)

# R has built-in functions to compute AIC
AIC(quiz_123)


# week10
#family = "binomial"
quiz_md = glm(y ~ ., data = quiz_data, family = "binomial")

# predict argument is newdata, not data!!!
mean(predict(pima_glm, data = Pima.te, type = "response") > 0.8)
mean(predict(pima_glm, newdata = Pima.te, type = "response") > 0.8)
#--------------------------------
x[-1]
which.max(x)
identical()
all.equal()
any
all

ifelse

# ctrl + shift + A=> reformat code

cbind()

rowSums()
colMeans()

str(example_data)

head(Galton, n = 10)

Galton[Galton$sex == "F", ]$height
# first ten rows with female height > 70
head(subset(Galton, subset = height > 70), n = 10)

table(mpg$class)

diff(pnorm(c(100, 115), mean = 100, sd = 15)

predict(stop_dist_model, newdata = data.frame(speed = 8)

predict(stop_dist_model)

unique(cars$speed)
8 %in% unique(cars$speed)

cars[which(cars$speed == 8)]

confint(stop_dist_model, level=0.99)

# abs!!!
pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE)

confint(tree_model, parm = "Height", level = 0.95)

hidden exptrapolation

length(coef(mpg_model))

# null first then full
anove(null_model, full_model)

1 - pf(0.5533, df1 = 4, df2 = 383)
