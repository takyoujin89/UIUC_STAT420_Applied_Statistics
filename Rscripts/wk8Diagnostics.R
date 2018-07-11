# 13 Model Diagnostics
# 13.2 Checking Assumptions
# 13.2.1 Fitted versus Residuals Plot
sim_1  = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x, y)
}

sim_2 = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x, y)
}

sim_3 = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2+ rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}  

set.seed(42)
sim_data_1 = sim_1()
head(sim_data_1)

plot(y ~ x, data = sim_data_1, col = "grey", pch = 20,
     main = "Data from Model 1")
fit_1 = lm(y ~ x, data = sim_data_1)
abline(fit_1, col = "darkorange", lwd = 3)

plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

set.seed(42)
sim_data_2 = sim_2()
fit_2 = lm(y ~ x, data = sim_data_2)
plot(y ~ x, data = sim_data_2, col = "grey", pch = 20, 
     main = "Data from Model 2")
abline(fit_2, col = "darkorange", lwd = 3)

# the linearity assumption isn't violated
# the constant variance assumpation is violated 
plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)


set.seed(42)
sim_data_3 = sim_3()

plot(y ~ x, data = sim_data_3, col = "grey", pch = 20,
     main = "Data from Model 3")
fit_3 = lm(y ~ x, data = sim_data_3)
abline(fit_3, col = "darkorange", lwd = 3)

# 13.2.2 bp test

# the constant variance assumption is met
# the linearity assumption is violated
plot(fitted(fit_3), resid(fit_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd = 2)

# H0: homoscedasticity
# H1: heteroscedasticity
install.packages("lmtest")
library(lmtest)

bptest(fit_1)
bptest(fit_2)
bptest(fit_3)

# 13.2.3 Histograms
par(mfrow = c(1, 3))
hist(resid(fit_1),
     xlab = "Residuals",
     main = "Histogram of Residuals, fit_1",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_2),
     xlab = "Residuals",
     main = "Histogram of Residuals, fit_2",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)
hist(resid(fit_3),
     xlab = "Residuals",
     main = "Histogram of Residuals, fit_3",
     col = "darkorange",
     border = "dodgerblue",
     breaks = 20)

# 13.2.4 Q-Q Plots
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1",
       col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

qq_plot = function(e) {
  
  n = length(e)
  normal_quantiles = qnorm(((1:n - 0.5) / n))
  # normal_quantiles = qnorm(((1:n) / (n + 1)))
  
  # plot theoretical verus observed quantiles
  plot(normal_quantiles, sort(e),
       xlab = c("Theoretical Quantiles"),
       ylab = c("Sample Quantiles"),
       col = "darkgrey")
  title("Normal Q-Q Plot")
  
  # calculate line through the first and third quartiles
  slope     = (quantile(e, 0.75) - quantile(e, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  intercept = quantile(e, 0.25) - slope * qnorm(0.25)
  
  # add to existing plot
  abline(intercept, slope, lty = 2, lwd = 2, col = "dodgerblue")
}

set.seed(42)
x = rnorm(100, mean = 0, sd = 1)
par(mfrow = c(1, 2))
qqnorm(x, col = "darkgrey")
qqline(x, lty = 2, lwd = 2, col = "dodgerblue")
qq_plot(x)

# qq_plot is good for a reasonable sample size
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rnorm(10))
qq_plot(rnorm(25))
qq_plot(rnorm(100))

# fat details 
# for sample size of 10 and 25, suspicious but not entirely confident
par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rt(10, df = 4))
qq_plot(rt(25, df = 4))
qq_plot(rt(100, df = 4))

par(mfrow = c(1, 3))
set.seed(420)
qq_plot(rexp(10))
qq_plot(rexp(25))
qq_plot(rexp(100))

# near perfect Q-Q plot, the errors follow a normal distribution
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

# suspicious
qqnorm(resid(fit_2), main = "Normal Q-Q Plot, fit_2", col = "darkgrey")
qqline(resid(fit_2), col = "dodgerblue", lwd = 2)

# suspicious
qqnorm(resid(fit_3), main = "Normal Q-Q Plot, fit_3", col = "darkgrey")
qqline(resid(fit_3), col = "dodgerblue", lwd = 2)


# 13.2.5 SW Test
set.seed(42)
shapiro.test(rnorm(25))

shapiro.test(rexp(25))

# notice shapiro.test is taking residuals as argument
# while bptest is taking model as argument
shapiro.test(resid(fit_1))
shapiro.test(resid(fit_1))$p.value
shapiro.test(resid(fit_2))
shapiro.test(resid(fit_3))


mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
plot(fitted(mpg_hp_add), resid(mpg_hp_add), col = "grey", pch = 20, cex = 2,
     xlab = "Fitted", ylab = "Residual",
     main = "mtcars: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

bptest(mpg_hp_add)

qqnorm(resid(mpg_hp_add), col = "darkgrey")
qqline(resid(mpg_hp_add), col = "dodgerblue", lwd = 2)

shapiro.test(resid(mpg_hp_add))


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
# remove the variable for name
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# create a dummary variable for foreign vs domestic cars. domestic = 1.
autompg$domestic = as.numeric(autompg$origin == 1)
# remove 3 and 5 cylinder cars (which are very rare.)
autompg = autompg[autompg$cyl != 5,]
autompg = autompg[autompg$cyl != 3,]
# the following line would verify the remaining cylinder possibilities are 4, 6, 8
#unique(autompg$cyl)
# change cyl to a factor variable
autompg$cyl = as.factor(autompg$cyl)

big_model = lm(mpg ~ disp * hp * domestic, data = autompg)
plot(fitted(big_model), resid(big_model), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",
     main = "mtcars: Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

bptest(big_model)

qqnorm(resid(big_model), col = "darkorange")
qqline(resid(big_model), col = "dodgerblue", lwd = 2)

shapiro.test(resid(big_model))


# 13.3 Unusual Observations
par(mfrow = c(1, 3))
set.seed(42)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)

#plot(y ~ x, data = ex_data)

# low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "grey",
     main = "Low Leverage, Large Residual, Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"), 
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "grey",
     main = "High Leverage, Small Residual, Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "grey", ylim = c(-3, 12),
     main = "High Leverage, Large Residual, Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

coef(ex_model)[2]
coef(model_1)[2]
coef(model_2)[2]
coef(model_3)[2]

# 13.3.1 Leverage
# The further away it's from mean x, the larger hi is
# large values of h1 indicate extreme values in X, leverages only depend on X

lev_ex = data.frame(
  x1 = c(0, 11, 11, 7, 4, 10, 5, 8),
  x2 = c(1, 5, 4, 3, 1, 4, 4, 2),
  y  = c(11, 15, 13, 14, 0, 19, 16, 8))

plot(x2 ~ x1, data = lev_ex, cex = 2)
points(7, 3, pch = 20, col = "red", cex = 2)

X = cbind(rep(1, 8), lev_ex$x1, lev_ex$x2)
H = X %*% solve(t(X) %*% X) %*% t(X)
diag(H)
sum(diag(H))

lev_fit = lm(y ~ ., data = lev_ex)
hatvalues(lev_fit)

coef(lev_fit)
which.max(hatvalues((lev_fit)))

lev_ex[which.max(hatvalues(lev_fit)),]

# if we modify the y value of the point with the highest leverage
# each coef changes in some way
lev_ex_1 = lev_ex
lev_ex_1$y[1] = 20
lm(y ~., data = lev_ex_1)

# what if we modify the y value of the point with the lowest leverage
# only the intercept has changed
lev_ex[which.min(hatvalues(lev_fit)),]
lev_ex_2 = lev_ex
lev_ex_2$y[4] = 30
lm(y ~., data = lev_ex_2)

# this point is the mean of both predictors
mean(lev_ex$x1)
mean(lev_ex$x2)

hatvalues(model_1)
hatvalues(model_2)
hatvalues(model_3)

hatvalues(model_1) > 2 * mean(hatvalues(model_1))
hatvalues(model_2) > 2 * mean(hatvalues(model_2))
hatvalues(model_3) > 2 * mean(hatvalues(model_3))

# 13.3.2 Outliers
resid(model_1)
rstandard(model_1)
rstandard(model_1)[abs(rstandard(model_1)) > 2]
resid(model_2)
rstandard(model_2)
# no points with large standardized residuals
rstandard(model_2)[abs(rstandard(model_2)) > 2]

rstandard(model_3)
rstandard(model_3)[abs(rstandard(model_3)) > 2]

cooks.distance(model_1)[11] > 4 / length(cooks.distance(model_1))
cooks.distance(model_2)[11] > 4 / length(cooks.distance(model_2))
cooks.distance(model_3)[11] > 4 / length(cooks.distance(model_3))

# 13.4 Data Analysis Examples
mpg_hp_add
plot(fitted(mpg_hp_add), resid(mpg_hp_add), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",
     main = "mtcars: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
# roughly constant
library(lmtest)
bptest(mpg_hp_add)

qqnorm(resid(mpg_hp_add), col = "darkgrey")
qqline(resid(mpg_hp_add), col = "dodgerblue", lwd = 2)
shapiro.test(resid(mpg_hp_add))

# two points of large leverage
sum(hatvalues(mpg_hp_add) > 2 * mean(hatvalues(mpg_hp_add)))

# only one with large residual
sum(abs(rstandard(mpg_hp_add) > 2))

cd_mpg_hp_add = cooks.distance(mpg_hp_add)
sum(cd_mpg_hp_add > 4 / length(cd_mpg_hp_add)) 
large_cd_mpg = cd_mpg_hp_add > 4 / length(cd_mpg_hp_add)
cd_mpg_hp_add[large_cd_mpg]
coef(mpg_hp_add)

# if we remove these two points
mpg_hp_add_fix = lm(mpg ~ hp + am, data = mtcars,
                    subset = cd_mpg_hp_add <= 4 / length(cd_mpg_hp_add))

coef(mpg_hp_add_fix)

par(mfrow = c(2, 2))
plot(mpg_hp_add)

# 13.4.2 Suspect Diagnostics
qqnorm(resid(big_model), col = "darkgrey")
qqline(resid(big_model), col = "dodgerblue", lwd = 2)

big_model_cd = cooks.distance(big_model)
sum(big_model_cd > 4 / length(big_model_cd))

big_model_fix = lm(mpg ~ disp * hp * domestic, data = autompg,
                   subset = big_model_cd < 4 / length(big_model_cd))
qqnorm(resid(big_model_fix), col = "grey")
qqline(resid(big_model_fix), col = "dodgerblue", lwd = 2)

# sw test fail to reject for a low alpha
shapiro.test(resid(big_model_fix))


# 14 Transformations
# 14.1 Response Transformation
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
initech_fit = lm(salary ~ years, data = initech)
summary(initech_fit)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))
plot(fitted(initech_fit), resid(initech_fit), col = "grey", pch = 20, 
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(initech_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit), col = "dodgerblue", lwd = 2)

initech_fit_log = lm(log(salary) ~ years, data = initech)
plot(log(salary) ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit_log, col = "darkorange", lwd = 2)

plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
# we see an expnoential relationship
curve(exp(initech_fit_log$coefficients[1] + initech_fit_log$coefficients[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)


par(mfrow = c(1, 2))
plot(fitted(initech_fit_log), resid(initech_fit_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted vs. Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(initech_fit_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(initech_fit_log), col = "dodgerblue", lwd = 2)

# compare RMSE
sqrt(mean(resid(initech_fit) ^ 2))

# the scale is wrong
sqrt(mean(resid(initech_fit_log) ^ 2))
sqrt(mean((initech$salary - exp(fitted(initech_fit_log))) ^ 2))

# from an additive model to a multiplicative model

# 14.1.2 Box-Cox Transformations

library(MASS)
library(faraway)
install.packages("faraway")

savings_model = lm(sr ~ ., data = savings)
boxcox(savings_model, plotit = TRUE)

boxcox(savings_model, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))

plot(fitted(savings_model), resid(savings_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)
# there likely are not any issue with the assumptions of this model
# which Breusch-Pagan and Shapiro-Wilk tests verify
bptest(savings_model)
shapiro.test(resid(savings_model))

gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model), resid(gala_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

gala_model_cox = lm((((Species ^ 0.3) - 1) / 0.3) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model_cox), resid(gala_model_cox), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# lambda = 0 in the interval
boxcox(initech_fit)

# 14.2 Predictor Transformation
par(mfrow = c(1, 2))
plot(mpg ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp = lm(mpg ~ hp, data = autompg)
abline(mpg_hp, col = "darkorange", lwd = 2)
plot(fitted(mpg_hp), resid(mpg_hp), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))
plot(log(mpg) ~ hp, data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp_log = lm(log(mpg) ~ hp, data = autompg)
abline(mpg_hp_log, col = "darkorange", lwd = 2)
plot(fitted(mpg_hp_log), resid(mpg_hp_log), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))
plot(log(mpg) ~ log(hp), data = autompg, col = "dodgerblue", pch = 20, cex = 1.5)
mpg_hp_loglog = lm(log(mpg) ~ log(hp), data = autompg)
abline(mpg_hp_loglog, col = "darkorange", lwd = 2)
plot(fitted(mpg_hp_loglog), resid(mpg_hp_loglog), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

# 14.2.1 Polynomials
plot(sales ~ advert, data = marketing,
     xlab = "Avert Spending (in $ 100,00)", ylab = "Sales (in $100,00)",
     pch = 20, cex = 2)

mark_mod = lm(sales ~ advert, data = marketing)
summary(mark_mod)
# To add the second order term we need to use the I() function 
mark_mod_poly2 = lm(sales ~ advert + I(advert ^ 2), data = marketing)
summary(mark_mod_poly2)

n = length(marketing$advert)
X = cbind(rep(1, n), marketing$advert, marketing$advert ^ 2)
t(X) %*% X
# same as summary$coefficients
solve(t(X) %*% X) %*% t(X) %*% marketing$sales

mark_mod_poly3 = lm(sales ~ advert + I(advert ^ 2) + I(advert ^ 3), data = marketing)
summary(mark_mod_poly3)

plot(sales ~ advert, data = marketing, 
     xlab = "Advert Spending (in $100,00)", ylab = "Sales (in $100,00)",
     pch = 20, cex = 2)
abline(mark_mod, lty = 2, col = "green", lwd = 2)
xplot = seq(0, 16, by = 0.01)
lines(xplot, predict(mark_mod_poly2, newdata = data.frame(advert = xplot)),
      col = "blue", lwd = 2)
lines(xplot, predict(mark_mod_poly3, newdata = data.frame(advert = xplot)),
      col = "red", lty = 3, lwd = 3)

library(ggplot2)

ggplot(data = marketing, aes(x = advert, y = sales)) +
  stat_smooth(method = "lm", se = FALSE, color = "green", formula = y ~ x) +
  stat_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x + I(x ^ 2)) +
  stat_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x + I(x ^ 2)+ I(x ^ 3)) +
  geom_point(colour = "black", size = 3)

set.seed(1234)
x = seq(0, 10)
y = 3 + x + 4 * x ^ 2 + rnorm(11, 0, 20)
plot(x, y, ylim = c(-300, 400), cex = 2, pch = 20)
fit = lm(y ~ x + I(x ^ 2))
# overfitting
fit_perfect = lm(y ~ x + I(x ^ 2) + I(x ^ 3) + I(x ^ 4) + I(x ^ 5) + I(x ^ 6)
                 + I(x ^ 7) + I(x ^ 8) + I(x ^ 9) + I(x ^ 10))
summary(fit_perfect)




xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit, newdata = data.frame(x = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_perfect, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)

library(readr)
econ <- read_csv("fuel_econ.csv")

plot_econ_curve = function(model){
  plot(mpg ~ mph, data = econ, xlab = "Speed (Miles per Hour)", 
       ylab = "Fuel Efficiency (Miles per Gallon)", col = "dodgerblue", 
       pch = 20, cex =2)
  xplot = seq(10, 75, by = 0.1)
  lines(xplot, predict(model, newdata = data.frame(mph = xplot)),
        col = "darkorange", lwd = 2, lty = 1)
}

fit1 = lm(mpg ~ mph, data = econ)

par(mfrow = c(1, 2))
plot_econ_curve(fit1)
plot(fitted(fit1), resid(fit1),xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

fit2 = lm(mpg ~ mph + I(mph ^ 2), data = econ)
summary(fit2)

par(mfrow = c(1, 2))
plot_econ_curve(fit2)
plot(fitted(fit2), resid(fit2),xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

fit3 = lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3), data = econ)
summary(fit3)

par(mfrow = c(1, 2))
plot_econ_curve(fit3)
plot(fitted(fit3), resid(fit3),xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

fit4 = lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4), data = econ)
summary(fit4)

par(mfrow = c(1, 2))
plot_econ_curve(fit4)
plot(fitted(fit4), resid(fit4), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

fit6 = lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4) + I(mph ^ 5) + I(mph^6), data = econ)
summary(fit6)

par(mfrow = c(1, 2))
plot_econ_curve(fit6)
plot(fitted(fit6), resid(fit6), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

anova(fit4, fit6)

fit8 = lm(mpg ~ mph + I(mph ^ 2) + I(mph ^ 3) + I(mph ^ 4) + I(mph ^ 5)
          + I(mph ^ 6) + I(mph ^ 7) + I(mph ^ 8), data = econ)
summary(fit8)

par(mfrow = c(1, 2))
plot_econ_curve(fit8)
plot(fitted(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals", 
     col = "dodgerblue", pch = 20, cex =2)
abline(h = 0, col = "darkorange", lwd = 2)

anova(fit6, fit8)

# quicker way to specify a model with many higher order terms
fit6_alt = lm(mpg ~ poly(mph, 6), data = econ)
all.equal(fitted(fit6), fitted(fit6_alt))

# different coef but same p-value
coef(fit6)
coef(fit6_alt)

fit6_alt2 = lm(mpg ~ poly(mph, 6, raw = TRUE), data = econ)
coef(fit6_alt2)

# 14.2.2 A Quadratic Model
sim_quad = function(sample_size = 500) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

set.seed(314)
quad_data = sim_quad(sample_size = 200)

lin_fit = lm(y ~ x, data = quad_data)
summary(lin_fit)

plot(y ~ x, data = quad_data, col = "grey", pch = 20, cex = 1.5,
     main = "Simulated Quadratic Data")
abline(lin_fit, col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))

plot(fitted(lin_fit), resid(lin_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lin_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lin_fit), col = "dodgerblue", lwd = 2)

quad_fit = lm(y ~ x + I(x^2), data = quad_data)
summary(quad_fit)

plot(y ~ x, data = quad_data, col = "grey", pch = 20, cex = 1.5,
     main = "Simulated Quadratic Data")
curve(quad_fit$coef[1] + quad_fit$coef[2] * x + quad_fit$coef[3] * x ^ 2,
      from = -5, to = 30, add = TRUE, col = "darkorange", lwd = 2)


par(mfrow = c(1, 2))

plot(fitted(quad_fit), resid(quad_fit), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(quad_fit), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(quad_fit), col = "dodgerblue", lwd = 2)

# 14.2.4 Comparing Polynomial Models
sim_higher = function(sample_size = 250){
  x = runif(n = sample_size, min = -1, max = 1) * 2
  y = 3 + -6 * x ^ 2 + 1 * x ^ 4 + rnorm(n = sample_size, mean = 0, sd = 3)
  data.frame(x, y)
}

set.seed(42)
data_higher = sim_higher()

plot(y ~ x, data  = data_higher, col = "grey", pch = 20, cex = 1.5,
     main = "Simulated Quartic Data")

fit_2 = lm(y ~ poly(x, 2), data = data_higher)
fit_4 = lm(y ~ poly(x, 4), data = data_higher)

plot(y ~ x, data = data_higher, col = "grey", pch = 20, cex = 1.5,
     main = "Simulated Quartic Data")
x_plot = seq(-5, 5, by = 0.05)
lines(x_plot, predict(fit_2, newdata = data.frame(x = x_plot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(x_plot, predict(fit_4, newdata = data.frame(x = x_plot)),
      col = "darkorange", lwd = 2, lty = 2)
par(mfrow = c(1, 2))

plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(fit_2), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(fit_2), col = "dodgerblue", lwd = 2)

par(mfrow = c(1, 2))

plot(fitted(fit_4), resid(fit_4), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(fit_4), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(fit_4), col = "dodgerblue", lwd = 2)

anova(fit_2, fit_4)

fit_6 = lm(y ~ poly(x, 6), data = data_higher)

anova(fit_4, fit_6)

# 14.2.5 poly()
fit_4a = lm(y ~ poly(x, degree = 4), data = data_higher)
fit_4b = lm(y ~ poly(x, degree = 4, raw = TRUE), data = data_higher)
fit_4c = lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = data_higher)

coef(fit_4a)
# b = c
coef(fit_4b)

coef(fit_4c)

unname(coef(fit_4a))
all.equal(fitted(fit_4b), fitted(fit_4c))
all.equal(resid(fit_4a), resid(fit_4b))

# 14.2.6 Inhibit Function
coef(lm(y ~ x + x ^ 2,data = quad_data))
# only this works
coef(lm(y ~ x + I(x ^ 2),data = quad_data))
coef(lm(y ~ x + x:x,data = quad_data))
coef(lm(y ~ x * x,data = quad_data))
coef(lm(y ~ x ^ x,data = quad_data))
coef(lm(y ~ I(x + x),data = quad_data))
coef(lm(y ~ x + x,data = quad_data))


pairs(autompg)
