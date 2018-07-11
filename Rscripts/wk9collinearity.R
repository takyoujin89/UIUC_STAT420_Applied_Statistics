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
library(faraway)
pairs(seatpos, col = "dodgerblue")

round(cor(seatpos), 2)

hip_model = lm(hipcenter ~ ., data = seatpos)
summary(hip_model)

ht_shoes_model = lm(HtShoes ~ . - hipcenter, data = seatpos)
summary(ht_shoes_model)$r.squared


#  variance inflation factor quantifies the effect of collinearity on the variance of our regression estimates
# When R2j is large, close to 1, xj is well explained by the other predictors

# vif from farawy package calculates the VIFs for each of the predictors of a model.
vif(hip_model)

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
abline(a = 0, b = 1, col = "darkorange", lwd = 2)

hip_model_small = lm(hipcenter ~ Age + Arm + Ht, data = seatpos)
summary(hip_model_small)
vif(hip_model_small)

anova(hip_model_small, hip_model)

# To quantify this effect we will look at a variable added plot and a partial correlation coefficient

ht_shoes_model_small = lm(HtShoes ~ Age + Arm + Ht, data = seatpos)
