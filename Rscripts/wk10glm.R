sim_logistic_data = function(sample_size = 25, beta_0 = -2, beta_1 = 3){
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1 / ( 1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}  
set.seed(1)
example_data = sim_logistic_data()
head(example_data)

fit_lm = lm(y ~ x, data = example_data)
# family = gaussian is default
fit_glm = glm(y ~ x, data = example_data, family = binomial)

fit_glm
fit_glm = glm(y ~ x, data = example_data, family = binomial(link = "logit"))

coef(fit_glm)
predict(fit_glm, newdata = data.frame(x = 1.2), type = "link")
predict(fit_glm, newdata = data.frame(x = 1.2), type = "response")
predict(fit_glm, newdata = data.frame(x = 1.2), type = "response")

# note that we're plotting the estimated mean
# we don't want probabilities less than 0 or greater than 1
plot(y ~ x, data = example_data,
     pch = 20, ylab = "Estimated Probability",
     main = "Ordinary vs Logistic Regression")
grid()
abline(fit_lm, col = "darkorange")
curve(predict(fit_glm, data.frame(x), type = "response"),
      add = TRUE, col = "dodgerblue", lty = 2)
legend("topleft", c("Ordinary", "Logistic", "Data"), lty = c(1, 2, 0), 
       pch = c(NA, NA, 20), lwd = 2, col = c("darkorange", "dodgerblue", "black"))


set.seed(1)
example_data = sim_logistic_data(sample_size = 50, beta_0 = 1, beta_1 = -4)
fit_glm = glm(y ~ x, data = example_data, family = binomial)

plot(y ~ x, data = example_data, 
     pch = 20, ylab = "Estimated Probability", 
     main = "Logistic Regression, Decreasing Probability")
grid()
curve(predict(fit_glm, data.frame(x), type = "response"), 
      add = TRUE, col = "dodgerblue", lty = 2)
legend("bottomleft", c("Estimated Probability", "Data"), lty = c(2, 0), 
       pch = c(NA, 20), lwd = 2, col = c("dodgerblue", "black"))


sim_quadratic_logistic_data = function(sample_size = 25){
  x = rnorm(n = sample_size)
  eta = -1.5 + 0.5 * x + x ^ 2
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}

set.seed(42)
example_data = sim_quadratic_logistic_data(sample_size = 50)


fit_glm = glm(y ~ x + I(x ^ 2), data = example_data, family = binomial)
plot(y ~ x, data = example_data,
     pch = 20,  ylab = "Estimated Probability", 
     main = "Logistic Regression, Quadratic Relationship")
grid()
curve(predict(fit_glm, data.frame(x), type = "response"),
      add = TRUE, col = "dodgerblue", lty = 2)
legend("left", c("Prob", "Data"), lty = c(2, 0),
       pch = c(NA, 20), lwd = 2, col = c("dodgerblue", "black"))

# 17.3.4 SAheart Example
install.packages("ElemStatLearn")
library(ElemStatLearn)
data("SAheart")
View(SAheart)
# Note that we have "jittered" the data to make it easier to visualize, but the data do only take values 0 and 1
chd_mod_ldl = glm(chd ~ ldl,data = SAheart, family = binomial)
plot(jitter(chd, factor = 0.1) ~ ldl, data = SAheart, pch = 20,
     ylab = "Probability of CHD", xlab = "Low Density Lipoprotein Cholesterol")
grid()
curve(predict(chd_mod_ldl, data.frame(ldl = x), type = "response"),
      add = TRUE, col = "dodgerblue", lty = 2)

coef(summary(chd_mod_ldl))

chd_mod_additive = glm(chd ~ ., data = SAheart, family = binomial)

-2 * as.numeric(logLik(chd_mod_ldl) - logLik(chd_mod_additive))

# LRT:  likelihood-ratio test
anova(chd_mod_ldl, chd_mod_additive, test = "LRT")

chd_mod_selected = step(chd_mod_additive, k = log(n))

coef(chd_mod_selected)

anova(chd_mod_selected, chd_mod_additive, test = "LRT")

# 17.3.5 Confidence Intervals
confint(chd_mod_selected, level = 0.99)

new_obs = data.frame(
  sbp = 148.0,
  tobacco = 5,
  ldl = 12,
  adiposity = 31.23,
  famhist = "Present",
  typea = 47,
  obesity = 28.50,
  alcohol = 23.89,
  age = 60
)

eta_hat = predict(chd_mod_selected, new_obs, se.fit = TRUE, type = "link")
eta_hat

# 0.975 = 1 - 0.05 / 2 
# for lm, crit = qt(0.995, df = length(resid(stop_dist_model)) - 2)
# 0.995 = 1 - 0.01 / 2
z_crit = round(qnorm(0.975), 2)
round(z_crit, 2)
eta_hat$fit + c(-1, 1) * z_crit * eta_hat$se.fit

boot::inv.logit(eta_hat$fit + c(-1, 1) * z_crit * eta_hat$se.fit)

# 17.3.7.1 Interactions
chd_mod_interaction = glm(chd ~ alcohol + ldl + famhist + typea + age + ldl:famhist,
                          data = SAheart, family = binomial)
summary(chd_mod_interaction)

# 17.3.7.2 Polynomial Terms
chd_mod_int_quad = glm(chd ~ alcohol + ldl + famhist + typea + age + ldl:famhist + I(ldl ^ 2),
                       data = SAheart, family = binomial)
# since this additional transformed variable wasn't intelligently chosen
summary(chd_mod_int_quad)

# 17.3.8 Deviance
# Like RSS, deviance decreased as the model complexity increases
deviance(chd_mod_ldl)

deviance(chd_mod_selected)

deviance(chd_mod_additive)

# 17.4 
# 17.4.1 spam Example
install.packages("kernlab")
library(kernlab)
data("spam")
View(spam)
tibble::as.tibble(spam)
is.factor(spam$type)
levels(spam$type)

set.seed(42)
nrow(spam) / 2
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]
nrow(spam_tst)

fit_caps = glm(type~ capitalTotal, data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar, data = spam_trn, family = binomial)
fit_additive = glm(type ~ ., data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.), data = spam_trn, family = binomial, maxit = 50)

coef(fit_selected)

# no argument=> predict the log odds
predict(fit_caps)
# with response => predict the probabilites
predict(fit_caps, type = "response")
# 
ifelse(predict(fit_caps, type = "response") > 0.5, "spam", "nonspam")
# same 
ifelse(predict(fit_caps) > 0, "spam", "nonspam")
all.equal(ifelse(predict(fit_caps, type = "response") > 0.5, "spam", "nonspam"),ifelse(predict(fit_caps) > 0, "spam", "nonspam"))


mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) >0, "spam", "nonspam") != spam_trn$type)

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]  # preferred
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

make_conf_mat = function(predicted, actual){
  table(predicted = predicted, actual = actual)
}

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type)
conf_mat_50

#             actual
# predicted nonspam spam
# nonspam    2050  161
# spam        137 1253
# spam is 1(true)
# 137: actual nonspam but predicted spam, false positive
# 161: actual spam but predicted nonspam, false negative
# sensitivity = true positive rate
# specificity = true negative rate

get_sens = function(conf_mat){
  conf_mat[2, 2] / sum(conf_mat[, 2])
}

get_spec = function(conf_mat){
  conf_mat[1, 1] / sum(conf_mat[, 1])
}

get_sens(conf_mat_50)
get_spec(conf_mat_50)

# prevalence
table(spam_tst$type) / nrow(spam_tst)
# overall the accuracy in the test set it
mean(spam_tst_pred == spam_tst$type)
# the test classification is
mean(spam_tst_pred != spam_tst$type)

# lower cutoff from 0.5 to 0.1
spam_tst_pred_10 = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.1,
                          "spam",
                          "nonspam")

(conf_mat_10 = make_conf_mat(predicted = spam_tst_pred_10, actual = spam_tst$type))
get_sens(conf_mat_10)
get_spec(conf_mat_10)

# greatly reduce false negatives, false positives have almost quadrupled
#         actual
# predicted nonspam spam
# nonspam    1654   31
# spam        533 1383

# change the cutoff to 0.9
spam_tst_pred_90 = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.9,
                          "spam",
                          "nonspam")
# We have far fewer false positives
(conf_mat_90 = make_conf_mat(predicted = spam_tst_pred_90, actual = spam_tst$type))
get_sens(conf_mat_90)
get_spec(conf_mat_90)


**The test statistic of the test is `r anova(tt_null, tt_mod, test = "LRT")[2, "Deviance"]` and the p-value is `r anova(tt_null, tt_mod, test = "LRT")[2, "Pr(>Chi)"]` which is smaller than 0.01, we reject the null hypothesis, so we prefer larger model.**