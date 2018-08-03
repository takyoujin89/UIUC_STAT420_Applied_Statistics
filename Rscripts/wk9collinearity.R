# 15.1 Exact Collinearity
gen_exact_collin_data = function(num_samples = 100){
  x1 = rnorm(n = num_samples, mean = 80, sd = 10)
  x2 = rnorm(n = num_samples, mean = 70, sd = 5)
  x3 = 2 * x1 + 4 * x2 + 3
  y = 3 + x1 + x2 + rnorm(n = num_samples, mean = 0, sd = 1)
  data.frame(y, x1, x2, x3)
}

set.seed(42)
exact_collin_data = gen_exact_collin_data()
head(exact_collin_data)

exact_collin_fit = lm(y ~ x1 + x2 + x3, data = exact_collin_data)
summary(exact_collin_fit)

# the whole matrix minus first column
exact_collin_data[,-1]

# this code won't run due to exact collinearity
X = cbind(1, as.matrix(exact_collin_data[,-1]))
solve(t(X) %*% X)

fit1 = lm(y ~ x1 + x2, data = exact_collin_data)
fit2 = lm(y ~ x1 + x3, data = exact_collin_data)
fit3 = lm(y ~ x2 + x3, data = exact_collin_data)

# the three models are same
all.equal(fitted(fit1), fitted(fit2))
all.equal(fitted(fit1), fitted(fit3))

coef(fit1)
coef(fit2)
coef(fit3)

# 15.2 Collinearity
install.packages()
library(faraway)
pairs(seatpos, col = "dodgerblue")

round(cor(seatpos), 2)

hip_model = lm(hipcenter ~ ., data = seatpos)
summary(hip_model)

# this shows that Ht's variation is largely explained by the other predictors,
ht_shoes_model = lm(HtShoes ~ . - hipcenter, data = seatpos)
summary(ht_shoes_model)$r.squared


#  variance inflation factor quantifies the effect of collinearity on the variance of our regression estimates
# When R2j is large, close to 1, xj is well explained by the other predictors

# vif from farawy package calculates the VIFs for each of the predictors of a model.
vif(hip_model)["Age"]

set.seed(1337)
noise = rnorm(n = nrow(seatpos), mean = 0, sd = 5)
hip_model_noise = lm(hipcenter + noise ~ ., data = seatpos)

# Adding random noise should not effect the coefficients of a model.
# but the sign of the coefficient for Ht changed
# it tells us a model with collinearity is bad at explaining the relationshipbetween the response and the predictors.
coef(hip_model)
coef(hip_model_noise)

plot(fitted(hip_model), fitted(hip_model_noise), col = "dodgerblue", pch = 20,
     xlab = "Predicted, Without Noise", ylab = "Predicted, With Noise", cex = 1.5)
# note a = 0, b = 1
abline(a = 0, b = 1, col = "darkorange", lwd = 2)

hip_model_small = lm(hipcenter ~ Age + Arm + Ht, data = seatpos)
summary(hip_model_small)
vif(hip_model_small)

# add noise again
set.seed(1337)
noise = rnorm(n = nrow(seatpos), mean = 0, sd = 5)
# without the collinearilty issue, the estimation is much stable
coef(hip_model_small)
coef(lm(hipcenter + noise ~ Age + Arm + Ht, data = seatpos))

anova(hip_model_small, hip_model)

# partial correlation

# To quantify this effect we will look at a variable added plot and a partial correlation coefficient

# Regressing the predictor of interest (HtShoes) against the other predictors (Age, Arm, and Ht).
ht_shoes_model_small = lm(HtShoes ~ Age + Arm + Ht, data = seatpos)

# the residuals of hip_model_small give us the variation of hipcenter that is unexplained by Age, Arm, and Ht
# the residuals of ht_shoes_model_small give us the variation of HtShoes unexplained by Age, Arm, and Ht
# this value is small shows very little correlation

cor(resid(ht_shoes_model_small), resid(hip_model_small))

# this plot shows that adding it to the model will not do much to improve the model
plot(resid(hip_model_small) ~ resid(ht_shoes_model_small),
	col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", 
     ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(hip_model_small) ~ resid(ht_shoes_model_small)),
  col = "darkorange", lwd = 2)

# 15.3 Simulation
set.seed(42)
beta_0 = 7
beta_1 = 3
beta_2 = 4
sigma = 5

sample_size = 10
num_sim = 2500

x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
x2 = c(1, 2, 3, 4, 5, 7, 6, 10, 9, 8)
c(sd(x1), sd(x2))
cor(x1, x2)

true_line_bad = beta_0 + beta_1 * x1 + beta_2 * x2
beta_hat_bad = matrix(0, num_sim, 2)
mse_bad = rep(0, num_sim)

for (s in 1:num_sim){
  y = true_line_bad + rnorm(n = sample_size, mean = 0, sd = sigma)
  reg_out = lm(y ~ x1 + x2)
  # betas except the intercept
  beta_hat_bad[s, ] = coef(reg_out)[-1]
  mse_bad[s] = mean(resid(reg_out) ^ 2)
}

# without collinearity
z1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
z2 = c(9, 2, 7, 4, 5, 6, 3, 8, 1, 10)

c(sd(z1), sd(z2))
cor(z1, z2)

true_line_good = beta_0 + beta_1 * z1 + beta_2 * z2
beta_hat_good = matrix(0, num_sim, 2)
mse_good = rep(0, num_sim)

for (s in 1:num_sim){
  y = true_line_good + rnorm(n = sample_size, mean = 0, sd = sigma)
  reg_out = lm(y ~ z1 + z2)
  beta_hat_good[s, ] = coef(reg_out)[-1]
  mse_good[s] = mean(resid(reg_out) ^ 2)
}

par(mfrow = c(1, 2))
hist(beta_hat_bad[, 1], 
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[1]* " with Collinearity"),
     xlab = expression(hat(beta)[1]),
     breaks = 20)
hist(beta_hat_good[, 1],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[1]* " without Collinearity"),
     xlab = expression(hat(beta)[1]),
     breaks = 20)

mean(beta_hat_bad[,1])
mean(beta_hat_good[,1])
# variance is still much larger in the simulations performed with collinearity
sd(beta_hat_bad[,1])
sd(beta_hat_good[,1])

par(mfrow = c(1, 2))
hist(beta_hat_bad[, 2],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[2]* " with Collinearity"),
     xlab = expression(hat(beta)[2]),
     breaks = 20)
hist(beta_hat_good[, 2],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[2]* " without Collinearity"),
     xlab = expression(hat(beta)[2]),
     breaks = 20)

mean(beta_hat_bad[,2])
mean(beta_hat_good[,2])
# variance is still much larger in the simulations performed with collinearity
sd(beta_hat_bad[,2])
sd(beta_hat_good[,2])


par(mfrow = c(1, 2))
hist(mse_bad,
     col = "darkorange",
     border = "dodgerblue",
     main = "MSE, with Collinearity",
     xlab = "MSE")
hist(mse_good,
     col = "darkorange",
     border = "dodgerblue",
     main = "MSE, without Collinearity",
     xlab = "MSE")

# the MSE is roughly the same on average
# this is because collinearity effects a model's ability to explain not predict
mean(mse_bad)
mean(mse_good)

# Chapter 16 Variable Selection and Model Building
# 16.1.4 Cross-Validated RMSE
make_poly_data = function(sample_size = 11){
  x = seq(0, 10)
  y = 3 + x + 4 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 20)
  data.frame(x, y)
}
set.seed(1234)
poly_data = make_poly_data()

fit_lin = lm(y ~ x, data = poly_data)
fit_quad = lm(y ~ poly(x, degree = 2), data = poly_data)
fit_big = lm(y ~ poly(x, degree = 8), data = poly_data)

plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad, newdata = data.frame(x = xplot)), col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)

sqrt(mean(resid(fit_lin) ^ 2))
sqrt(mean(resid(fit_quad) ^ 2))
sqrt(mean(resid(fit_big) ^ 2))


calc_loocv_rmse = function(model){
  sqrt(mean((resid(model)/ (1 - hatvalues(model))) ^ 2))
}

calc_loocv_rmse(fit_quad)
calc_loocv_rmse(fit_big)

remove = 2
fit_quad_removed = lm(y ~ poly(x, degree = 2), data = poly_data[-remove, ])
fit_big_removed = lm(y ~ poly(x, degree = 8), data = poly_data[-remove, ])

# the quadratic model doesn't change much, the big model changes a lot
plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad_removed, newdata = data.frame(x = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big_removed, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)

# 16.2 Selection Procedures
library(faraway)
hipcenter_mod = lm(hipcenter ~ ., data = seatpos)
coef(hipcenter_mod)

# 16.2.1 Backward Search

hipcenter_md_back_aic = step(hipcenter_mod, direction = "backward")

extractAIC(hipcenter_mod)

n = length(resid(hipcenter_mod))
(p = length(coef(hipcenter_mod)))

# verify the AIC
n * log(mean(resid(hipcenter_mod) ^ 2)) + 2 * p

# backward using BIC
n = length(resid(hipcenter_mod))
# this stores the model chosen by this procedure
hipcenter_md_back_bic = step(hipcenter_mod, direction = "backward", k = log(n))

coef(hipcenter_md_back_bic)
coef(hipcenter_md_back_aic)

# the original full model has the lowest adj r2 => performs worst 
summary(hipcenter_mod)$adj.r.squared
summary(hipcenter_md_back_aic)$adj.r.squared
summary(hipcenter_md_back_bic)$adj.r.squared

# will prefer bic model for LOOCV metric
calc_loocv_rmse(hipcenter_mod)
calc_loocv_rmse(hipcenter_md_back_aic)
calc_loocv_rmse(hipcenter_md_back_bic)

# 16.2.2 Forward Search
hipcenter_mod_start = lm(hipcenter ~ 1, data = seatpos)
hipcenter_mod_forw_aic = step(hipcenter_mod_start, 
                              scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
                              direction = "forward")

hipcenter_mod_forw_bic = step(hipcenter_mod_start, 
                              scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
                              direction = "forward", k = log(n))

summary(hipcenter_mod)$adj.r.squared
summary(hipcenter_mod_forw_aic)$adj.r.squared
summary(hipcenter_mod_forw_bic)$adj.r.squared

calc_loocv_rmse(hipcenter_mod)
calc_loocv_rmse(hipcenter_mod_forw_aic)
calc_loocv_rmse(hipcenter_mod_forw_bic)

# 16.2.3 Stepwise Search
hipcenter_mod_both_aic = step(hipcenter_mod_start,
                              scope = hipcenter ~  Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
                              direction = "both")
hipcenter_mod_both_bic = step(hipcenter_mod_start,
                              scope = hipcenter ~  Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
                              direction = "both", k = log(n))

summary(hipcenter_mod)$adj.r.squared
summary(hipcenter_mod_both_aic)$adj.r.squared
summary(hipcenter_mod_both_bic)$adj.r.squared

calc_loocv_rmse(hipcenter_mod)
calc_loocv_rmse(hipcenter_mod_both_aic)
calc_loocv_rmse(hipcenter_mod_both_bic)
# 16.2.4 Exhaustive Search

install.packages("leaps")
library(leaps)
all_hipcenter_mod = summary(regsubsets(hipcenter ~., data = seatpos))
all_hipcenter_mod
all_hipcenter_mod$which

names(summary(regsubsets(hipcenter ~., data = seatpos)))
all_hipcenter_mod$rss
all_hipcenter_mod$adjr2
(best_r2_ind = which.max(all_hipcenter_mod$adjr2))
all_hipcenter_mod$which[best_r2_ind, ]

p = length(coef(hipcenter_mod))
n = length(resid(hipcenter_mod))

hipcenter_mod_aic = n * log(all_hipcenter_mod$rss / n ) + 2 * (2:p)

best_aic_ind = which.min(hipcenter_mod_aic)
all_hipcenter_mod$which[best_aic_ind, ]

hipcenter_mod_best_aic = lm(hipcenter ~ Age + Ht + Leg, data = seatpos)
# same model
extractAIC(hipcenter_mod_best_aic)
extractAIC(hipcenter_mod_forw_aic)
extractAIC(hipcenter_mod_both_aic)

plot(hipcenter_mod_aic ~ I(2:p), ylab = "AIC", xlab = "p, number of parameters", 
     pch = 20, col = "dodgerblue", type = "b", cex = 2,
     main = "AIC vs Model Complexity")

hipcenter_mod_bic = n * log(all_hipcenter_mod$rss / n) + log(n) * (2 : p)
which.min(hipcenter_mod_bic)

all_hipcenter_mod$which[1, ]

hipcenter_mod_best_bic = lm(hipcenter ~ Ht, data = seatpos)

extractAIC(hipcenter_mod_best_bic, k = log(n))
extractAIC(hipcenter_md_back_bic, k = log(n))
extractAIC(hipcenter_mod_forw_bic, k = log(n))
extractAIC(hipcenter_mod_both_bic, k = log(n))
#--------------------------------
# 16.3 Higher Order Terms

autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", 
                      "year", "origin", "name")
autompg = subset(autompg, autompg$hp != "?")
autompg = subset(autompg, autompg$name != "plymouth reliant")
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
autompg$hp = as.numeric(autompg$hp)
autompg$domestic = as.numeric(autompg$origin == 1)
autompg = autompg[autompg$cyl != 5,]
autompg = autompg[autompg$cyl != 3,]
autompg$cyl = as.factor(autompg$cyl)
autompg$domestic = as.factor(autompg$domestic)
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", 
                                     "wt", "acc", "year", "domestic"))

# log(mpg) ~ . ^ 2 will automatically consider all first-order terms as well as all two-way interactions
# We use I(var_name ^ 2) to add quadratic terms for some variables
# This generally works better than using poly() when performing variable selection.

pairs(autompg, col= "dodgerblue")

autompg_mod = lm(log(mpg) ~., data = autompg)

autompg_big_mod = lm(log(mpg) ~ . ^ 2 + I(disp ^ 2) + I(hp ^ 2) + I(wt ^ 2) + I(acc ^ 2), data = autompg)
autompg_mod_big = lm(log(mpg) ~ . ^ 2 + I(disp ^ 2) + I(hp ^ 2) + I(wt ^ 2) + I(acc ^ 2), data = autompg)

length(coef(autompg_mod_big))

#  used trace = 0 in the function call
# This suppress the output for each step, and simply stores the chosen model. 
autompg_mod_back_aic = step(autompg_mod_big, direction = "backward", trace = 0)

n = length(resid(autompg_big_mod))
autompg_mod_back_bic = step(autompg_big_mod, direction = "backward", k = log(n), trace = 0)

# step funciton respects hierarchy
coef(autompg_mod_back_aic)
length(coef(autompg_mod_back_aic))
coef(autompg_mod_back_bic)
length(coef(autompg_mod_back_bic))

calc_loocv_rmse(autompg_big_mod)
calc_loocv_rmse(autompg_mod_back_aic)
calc_loocv_rmse(autompg_mod_back_bic)

autompg_mod_forw_start = lm(log(mpg) ~ 1, data = autompg)
# start with adding first order term, after add wt and year, the wt:year interaction variable shows up
# cyl variable includes cyl4, cyl6, cyl8
autompg_mod_forw_aic = step(autompg_mod_forw_start, scope = ~ (cyl + disp + hp + wt + acc + year + domestic) ^ 2,
                            direction = "forward")

# week 9 homework


birthday = 19870725
set.seed(birthday)
num_sim = 300
false_neg_aic = matrix(0, num_sim, 5)
false_pos_aic = matrix(0, num_sim, 5)
false_neg_bic = matrix(0, num_sim, 5)
false_pos_bic = matrix(0, num_sim, 5)

x_1  = runif(n, 0, 10)
x_2  = runif(n, 0, 10)
x_3  = runif(n, 0, 10)
x_4  = runif(n, 0, 10)
x_5  = runif(n, 0, 10)
x_6  = runif(n, 0, 10)
x_7  = runif(n, 0, 10)
x_8  = runif(n, 0, 10)
x_9  = runif(n, 0, 10)
x_10 = runif(n, 0, 10)

y = rep(0, n)
sim_data_1 = data.frame(y, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10)


for (i in 1: num_sim){
  sim_data_1$y = beta_0 + beta_1 * x_1 + beta_2 * x_2 + beta_3 * x_3 + beta_4 * x_4 + 
      beta_5 * x_5 + rnorm(n, 0 , sigma)  
  
  mod_all = lm(y ~ ., data = sim_data_1)
  mod_back_aic = step(mod_all, direction = "backward", trace = 0)
  mod_back_bic = step(mod_all, direction = "backward", k = log(n), trace = 0)
  
  false_neg_aic[i, ] = !(signif %in% names(coef(mod_back_aic)))
  false_pos_aic[i, ] = not_sig %in% names(coef(mod_back_aic))
  false_neg_bic[i, ] = !(signif %in% names(coef(mod_back_bic)))
  false_pos_bic[i, ] = not_sig %in% names(coef(mod_back_bic))
  
}
fn_aic = sum(false_neg_aic) / (num_sim * 5)
fp_aic = sum(false_pos_aic) / (num_sim * 5)

fn_bic = sum(false_neg_bic) / (num_sim * 5)
fp_bic = sum(false_pos_bic) / (num_sim * 5)

abtable = rbind(c("AIC", fn_aic, fp_aic), c("BIC", fn_bic, fp_bic))
library(knitr)
library(kableExtra)
compare = kable(abtable, format = "html", col.names = c(" ", "False Negative", "False Positive"), caption = "Compare AIC and BIC")%>% kable_styling("striped")
compare
