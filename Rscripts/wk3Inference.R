# 8.2 Sampling Distributions
summary(stop_dist_model)

plot(dist ~ speed, data = cars,
     xlab = "Speed ( in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey")
abline(stop_dist_model, lwd = 5, col = "darkorange")

set.seed(42)
sample_size = 100
x = seq(-1, 1, length = sample_size)
Sxx = sum((x - mean(x)) ^ 2)

beta_0 = 3
beta_1 = 6
sigma = 2

var_beta_1_hat = sigma ^ 2/ Sxx
var_beta_1_hat
var_beta_0_hat = sigma ^ 2 * (1 / sample_size + mean(x) / Sxx)
var_beta_0_hat

num_samples = 10000
# it's good practice to preallocate vectors 
beta_0_hats = rep(0, num_samples)
beta_1_hats = rep(0, num_samples)

for (i in 1:num_samples){
  eps = rnorm(sample_size, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + eps
  sim_model = lm(y ~ x)
  beta_0_hats[i] = coef(sim_model)[1]
  beta_1_hats[i] = coef(sim_model)[2]
  
}

mean(beta_1_hats)
beta_1
mean(beta_0_hats)
beta_0
var(beta_1_hats)
var_beta_1_hat

# the histogram is generated based on empirical distribution
hist(beta_1_hats, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")
# the curve is for the true normal distribution, and it matches our histogram
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)), 
      col = "darkorange", add = TRUE, lwd = 3)

# just pick one value(6.2)from the two distributions, their locations are extremely close
mean(beta_1_hats < 6.2)
pnorm(6.2, mean = beta_1, sd = var_beta_1_hat)

# the more and more simulations we get, the closer and closer we get to the true mean
par(mar = c(5, 5, 1, 1))
plot(cumsum(beta_1_hats) / (1:length(beta_1_hats)), type = "l", ylim = c(5.95, 6.05),
     xlab = "Numer of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[1]), 
     col = "dodgerblue")
abline(h = 6, col = "darkorange", lwd = 2)

par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_0_hats) / (1:length(beta_0_hats)), type = "l", ylim = c(2.95, 3.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[0]),
     col  = "dodgerblue")
abline(h = 3, col = "darkorange", lwd = 2)

# 8.3 Standard Errors 
# t Distribution
# standard errors are estimated standard deviation

x = seq(-4, 4, length = 100)

plot(x,dnorm(x), type = "l", lty = 1, lwd = 2,
     xlab = "x", ylab = "Density", main = "Normal vs t Distribution")
     lines(x, dt(x, df = 1), lyt = 3, lwd = 2, col = "darkorange")
     lines(x, dt(x, df = 10), lyt = 2, lwd = 2, col = "dodgerblue")
     
legend("topright", title = "Distributions", 
       legend = c("t, df = 1", "t, df = 10", "Standard Normal" ),
       lwd = 2, lty = c(3, 2, 1), col = c("darkorange", "dodgerblue", "black"))

# 8.4 Confidence Intervals for Slope and Intercept
# point estimates => interval estimates

# qt() function is to calculate critical values
# confint() and predict()


stop_dist_model
plot(dist ~ speed, data = cars,
     xlab = "Speed in Miles Per Hour",
     ylab = "Stopping Distance in Feet",
     main = "Stopping Distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey"
)
# lwd = line width, lty = line type
abline(stop_dist_model, lwd = 3, col = "darkorange")

confint(stop_dist_model, level = 0.99)
# extract particular value from the CI matrix
confint(stop_dist_model, level = 0.99)[2,2]

confint(stop_dist_model, parm = "(Intercept)", level = 0.99)

confint(stop_dist_model, parm = "speed", level = 0.99) # the slope

# shorter interval
confint(stop_dist_model, parm = "speed", level = 0.95)

summary(stop_dist_model)

# store estimate
beta_1_hat = coef(stop_dist_model)[2]
beta_1_hat
# store standard error
beta_1_hat_se = summary(stop_dist_model)$coef[2,2]
beta_1_hat_se

# verify standard error
Sxx = sum((cars$speed - mean(cars$speed)) ^ 2)
s_e = summary(stop_dist_model)$sigma
s_e / sqrt(Sxx)
all.equal(s_e / sqrt(Sxx), beta_1_hat_se)

# calculate critical value for the two-sided 99%
(1 - 0.99) / 2
# 0.995 = 1 -  (1 - 0.99) / 2
# qt gives the quantile for t distribution for a probability 0.995
crit = qt(0.995, df = nrow(cars) - 2)
crit

# est - margin, est + margin
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)
# 2.817919 and 5.046899 match with below
confint(stop_dist_model, parm = "speed", level = 0.99) 

# pt() vs qt(), p for probability, q for quantile
pt(qt(0.995, df = nrow(cars) - 2), df = nrow(cars) - 2)  # 0.995
# alpha
1 - pt(qt(0.995, df = nrow(cars) - 2), df = nrow(cars) - 2) 


new_speeds = data.frame(speed = c(5, 21))
# point estimates
predict(stop_dist_model, newdata = new_speeds)

# CI for mean response
predict(stop_dist_model, newdata = new_speeds,
        interval = c("confidence"), level = 0.99)

#    fit       lwr      upr
# 1  2.082949 -10.89309 15.05898   # CI for the mean response x = 5
# 2 65.001489  56.45836 73.54462   # CI for the mean response x = 21


# PI for new observation
predict(stop_dist_model, newdata = new_speeds,
        interval = c("prediction"), level = 0.99)
# wider intervals
#  fit       lwr       upr
# 1  2.082949 -41.16099  45.32689
# 2 65.001489  22.87494 107.12803

speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)

dist_ci_band = predict(stop_dist_model,
                       newdata = data.frame(speed = speed_grid),
                       interval = "confidence", level = 0.99)

dist_pi_band = predict(stop_dist_model,
                       newdata = data.frame(speed = speed_grid),
                       interval = "prediction", level = 0.99)

plot(dist ~ speed, data = cars,
     xlab = "Speed in Miles Per Hour",
     ylab = "Stopping Distance in Feet",
     main = "Stopping Distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey",
     ylim = c(min(dist_pi_band), max(dist_pi_band)))
abline(stop_dist_model, lwd = 5, col= "darkorange")

lines(speed_grid, dist_ci_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_ci_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_pi_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_pi_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)

# extract summary information
names(summary(stop_dist_model))
summary(stop_dist_model)$coefficients
summary(stop_dist_model)$coefficients[2, 4]
summary(stop_dist_model)$coefficients["speed", "Pr(>|t|)"]  #  1.489836e-12 is very small => reject H0

# store test statistics
beta_0_hat_t = summary(stop_dist_model)$coefficients["(Intercept)", "t value"]
beta_0_hat_t
beta_1_hat_t = summary(stop_dist_model)$coefficients["speed", "t value"]
beta_1_hat_t
# verify p-values
2 * pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE)
2 * pt(-abs(beta_1_hat_t), df = nrow(cars) - 2)
pt(-abs(beta_1_hat_t), df = nrow(cars) - 2) + pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE)


# simulate SLR
sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1){
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

set.seed(1)
x = seq(1, 20, length.out = 21)
sim_data = sim_slr(x = x, beta_0 = 2, beta_1 = 0, sigma = 1)

sim_fit = lm(response ~ predictor, data = sim_data)
summary(sim_fit)$coefficients["predictor", "Pr(>|t|)"]

plot(response ~ predictor, data = sim_data, pch = 20, col = "gray", cex = 1.5)
abline(sim_fit, col = "darkorange", lwd = 3)
abline(2, 0, lwd = 3, lty = 2, col = "dodgerblue")

# 0.02387671 => reject H0, type I error

# 0.1441619 => fail to reject H0, type II error
x = seq(1, 20, length.out = 21)
sim_data = sim_slr(x = x, beta_0 = 2, beta_1 = 0.5, sigma = 7)

sim_fit = lm(response ~ predictor, data = sim_data)
summary(sim_fit)$coefficients["predictor", "Pr(>|t|)"]

plot(response ~ predictor, data = sim_data, pch = 20, col = "gray", cex = 1.5)
abline(sim_fit, col = "darkorange", lwd = 3)
abline(2, 0, lwd = 3, lty = 2, col = "dodgerblue")


# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals

# Calculating Confidence Intervals