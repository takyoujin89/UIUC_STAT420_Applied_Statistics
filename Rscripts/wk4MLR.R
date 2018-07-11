# rm(list=ls()) to clean the environment


# 9.1 Matrix Approach to Regression
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# check final structure of data
str(autompg)

View(autompg)

mpg_model = lm(mpg ~ wt + year, data = autompg)
coef(mpg_model)
summary(mpg_model)

n = nrow(autompg)
p = length(coef(mpg_model))
X = cbind(rep(1, n), autompg$wt, autompg$year)
y = autompg$mpg

X
t(X) %*% X

# solve() returns the inverse
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y

beta_hat
y_hat = X %*% beta_hat

e = y - y_hat
e

# why s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p))) == sqrt(t(e) %*% e / (n - p)) 
# e is one-col matrix, e ^ 2 => e T * e => 1Xn matrix * nx1 matrix => 1x1

sqrt(sum((y - y_hat) ^ 2) / (n - p))  #  3.431367

sqrt(t(e) %*% e / (n - p))  #  3.431367

summary(mpg_model)$sigma  #  3.431367

mpg_model_small = lm(mpg ~ year, data = autompg)

# different for SLR and MLR because of including wt
coef(mpg_model)
coef(mpg_model_small)

summary(mpg_model)$r.squared  # 0.8082355
summary(mpg_model_small)$r.squared  # 0.3356825
# using fewer variables, less of the variable in miles per gallon is explained


# use `r ` to update the result in RMD \hat{\beta}_2 = `r coef(mpg_model)[3]`

# not only maginitude of coef changes, but also sign of it
coef(lm(mpg ~ acc, data = autompg))  # 1.206107 
coef(lm(mpg ~ acc + hp, data = autompg))  # -0.6071746  -0.1877986 

# model1:acceleration increases, mpg increase
# So a larger value is a slower car, a smaller value is a faster car
# slower cars often as sort of maybe more fuel efficient like spots car are generally not very fuel sufficient but they're faster

# model2:for a particular horse power, if the acceleration is small then we're probably dealing with something like a sports car that's fast. Where is if the acceleration is large that is a long time to 60 miles per hour we're probably dealing with a truck or a a very large car.
# it makes sense that the lighter sports car probably, actually, has better fuel efficiency. So when we move from a low acceleration to a high acceleration, we actually see a decrease in fuel efficiency.


# 9.2 9.2 Sampling Distribution

mpg_model = lm(mpg ~ wt + year, data = autompg)
summary(mpg_model)

summary(mpg_model)$coef
summary(mpg_model)$coef["year", "Pr(>|t|)"]

confint(mpg_model, level = 0.99)

new_cars = data.frame(wt = c(3500, 5000), year = c(76, 81))
new_cars

# CI for mean response
predict(mpg_model, newdata = new_cars, interval = "confidence", level = 0.99)
# 1 20.00684 19.4712 20.54248
# 2 13.86154 12.3341 15.38898


# PI for new observation
predict(mpg_model, newdata = new_cars, interval = "prediction", level = 0.99)

# fit       lwr      upr
# 1 20.00684 11.108294 28.90539
# 2 13.86154  4.848751 22.87432

# in the range of datasest
new_cars$wt
range(autompg$wt)
new_cars$year
range(autompg$year)

plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)
# the second point is outside of the observed values, considered as extrapolation
points(new_cars, col = "darkorange", cex = 3, pch = 'X')

beta_hat
x1 = c(1, 3500, 76)
x1 %*% beta_hat  # 20.00684
x2 = c(0, 0 , 1)
x2 %*% beta_hat  # 0.761402

confint(mpg_model, level = 0.99, parm = "wt")
summary(mpg_model)$coef
est = summary(mpg_model)$coef["wt", "Estimate"]
est
se = summary(mpg_model)$coef["wt", "Std. Error"]
se

nrow(autompg)  # n
length(coef(mpg_model)) # number of parameters

# df = n - p = 390 - 3 = 387
(1 - 0.99) / 2
crit = abs(qt(0.005, df = 387))
crit

c(est - crit * se, est + crit * se)
confint(mpg_model, level = 0.99, parm = "wt")

# 9.5 Simulation
set.seed(1337)
n = 100  # sample size
p = 3
beta_0= 5
beta_1 = -2
beta_2 = 6
sigma = 4

x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)

View(X)
C[3, 3]
C[2 + 1, 2 + 1]
sigma ^ 2 * C[2 + 1, 2 + 1]
sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])

y = rep(0, n)
num_sims = 10000
beta_hat_2 = rep(0, num_sims)

for (i in 1:num_sims){
  eps = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
  fit = lm(y ~ x1 + x2)
  beta_hat_2[i] = coef(fit)[3]
}

mean(beta_hat_2)
var(beta_hat_2)
sigma ^ 2 *C[2 + 1, 2 + 1]
sd(beta_hat_2)
sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])

hist(beta_hat_2, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

sim_beta_hat_2 = function(){
  eps = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit = lm(y ~ x1 + x2)
  coef(fit)[3]
}

sim_beta_hat_2()

replicate(n = num_sims, sim_beta_hat_2())

beta_hat_2_alt = replicate(n = num_sims, sim_beta_hat_2())
beta_hat_2_alt

system.time(
  for (i in 1:num_sims){
    eps = rnorm(n, mean = 0, sd = sigma)
    y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
    fit = lm(y ~ x1 + x2)
    beta_hat_2[i] = coef(fit)[3]
  }
)

system.time(
  {beta_hat_2_alt = replicate(n = num_sims, sim_beta_hat_2())}
)  

# 9.4 Nested Models
names(autompg)
null_mpg_model = lm(mpg ~ wt + year, data = autompg)
full_mpg_model = lm(mpg ~ ., data = autompg)

# another set of comparison
# null_mpg_model = lm(mpg ~ 1, data = autompg)
# full_mpg_model = lm(mpg ~ wt + year, data = autompg)

anova(nulll_mpg_model, full_mpg_model)
# F = 0.5533, p = 0.6967 > 0.05, fail to reject, prefer small model

# SSDiff
sum((fitted(full_mpg_model) - fitted(null_mpg_model)) ^ 2) 

# SSE(For Full)
sum(resid(full_mpg_model) ^ 2)

# SST(For Null)
sum(resid(null_mpg_model) ^ 2)

# DF: Diff
length(coef(full_mpg_model)) - length(coef(null_mpg_model))

# DF: Full
length(resid(full_mpg_model)) - length(coef(full_mpg_model))

# DF: Null
length(resid(null_mpg_model)) - length(coef(null_mpg_model))

# df1 = p - q = 4,  df2 = n - p

# why 1 - pf, since we want F > some and p-value < some, use upper.tail
# but by default, pf and qf function set lower.tail = True
# if don't use 1 -, we could use lower.tail = FALSE
1 - pf(0.5533, df1 = 4, df2 = 383)
nrow(autompg)  # n = 390
length(coef(full_mpg_model))  # p = 7
 
# 9.5 Simulation

set.seed(1337)
n = 100
p = 3

beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma = 4

x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)

X

eps = rnorm(n, mean = 0, sd = sigma)
y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
sim_data = data.frame(x1, x2, y)

(beta_hat = C %*% t(X) %*% y)

coef(lm(y ~ x1 + x2, data = sim_data))
c(beta_0, beta_1, beta_2)

y_hat = X %*% beta_hat

(s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p)))

C[3, 3]

sigma ^ 2 * C[2 + 1, 2 + 1]

num_sims = 10000
beta_hat_2 = rep(0, num_sims)
for (i in 1:num_sims){
  eps = rnorm(n, mean = 0, sd = sigma)
  sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit = lm(y ~ x1 + x2, data = sim_data)
  beta_hat_2[i] = coef(fit)[3]
}

mean(beta_hat_2)  # beta_2 = 2
var(beta_hat_2)  #sigma ^ 2 * C[2 + 1, 2 + 1] 
sd(beta_hat_2)  # sqrt(sigma ^ 2 * C[2 + 1, 2 + 1] )

hist(beta_hat_2, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2+1, 2+1])), 
      col = "darkorange", add = TRUE, lwd = 3)
# verify 68-95-99.7 rule
sd_bh2 = sqrt(sigma ^ 2 * C[2+1, 2+1])
mean(beta_2 - 1 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 1 * sd_bh2)

mean(beta_2 - 2 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 2 * sd_bh2)

mean(beta_2 - 3 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 3 * sd_bh2)
# don't do this
# beta_hat = NULL
# beta_hat_2 = c(beta_hat_2, coef(fit)[3])

# 10 Modeling
