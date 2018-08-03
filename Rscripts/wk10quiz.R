# Practice
# 1
eta = 2 + -1 * 1 + -1 * 0
1 / ( 1 + exp(-eta))
#  0.7310586

# 2
fit_glm = glm(y ~ ., data = quiz_data, family = binomial(link = "logit"))
coef(fit_glm)
# 0.8839991

# 3
fit_glm = glm(y ~ ., data = quiz_data, family = binomial(link = "logit"))
summary(fit_glm)
summary(fit_glm)$coefficients["x3", "Pr(>|z|)"]
# 0.2395128

# 4
# quiz_data
fit_glm = glm(y ~ ., data = quiz_data, family = binomial)
summary(fit_glm)
fit_select = step(fit_glm, trace = 0)
anova(fit_glm, fit_select, test = "LRT")
# 0.7695132

# 5
#quiz_data
fit_glm = glm(y ~ ., data = quiz_data, family = binomial)
fit_select = step(fit_glm, trace = 0)
set.seed(1)
library(boot)
cv.glm(quiz_data, fit_select, K = 5)$delta[1]
# 0.1267324


# Quiz
# 1
eta = -3 + 1 * -1 + 2 * 0.5 + 3 * 0.25
# note it's the probability of Y = 0 
1 - 1 / (1 + exp(-eta))
# 0.9046505

# 2
View(mtcars)
car_md = glm(am ~ mpg + hp + qsec, data = mtcars, family = binomial)
coef(car_md)
coef(car_md)["qsec"]
# -4.040952 

# 3
coef(car_md)["mpg"]
# 2.29643 


# 4
predict(car_md, newdata = data.frame(mpg = 19, hp = 150, qsec = 19), type = "link")

# -8.338686 

# 5
predict(car_md, newdata = data.frame(mpg = 22, hp = 123, qsec = 18), type = "response")
# 0.916414 

# 6
car_md = glm(am ~ mpg + hp + qsec, data = mtcars, family = binomial)
car_null = glm(am ~ 1, data = mtcars, family = binomial)
anova(car_null, car_md, test = "LRT")
anova(car_null, car_md, test = "LRT")[2, "Deviance"]
# 35.74953



# 7
summary(car_md)$coefficients["hp", "Pr(>|z|)"]
# 0.8771707

# 8
View(MASS::Pima.tr)
dia_glm = glm(type ~ glu + ped + I(glu ^ 2) + I(ped ^ 2) + glu : ped, data = MASS::Pima.tr, family = binomial)
coef(dia_glm)
coef(dia_glm)["I(ped^2)"]

# -0.3595626 

# 9
predict(dia_glm, MASS::Pima.te, type = "response") 
mean(predict(dia_glm, MASS::Pima.te, type = "response") > 0.8)
# 0.07228916

# 10
dia_add = glm(type ~ ., data = MASS::Pima.tr, family = binomial)
dia_reduce = step(dia_add, trace = 0)
length(coef(dia_reduce)) - 1
# 5

#11
dia_full = glm(type ~ . ^ 2, data = MASS::Pima.tr, family = binomial)
dia_select = step(dia_full, trace = 0)
deviance(dia_select)
# 162.6924

# 12
library(MASS)
library(boot)
set.seed(42)
cv.glm(MASS::Pima.tr, dia_glm, K = 5)$delta[1]
set.seed(42)
cv.glm(MASS::Pima.tr, dia_add, K = 5)$delta[1]
set.seed(42)
cv.glm(MASS::Pima.tr, dia_reduce, K = 5)$delta[1]
set.seed(42)
cv.glm(MASS::Pima.tr, dia_full, K = 5)$delta[1]
set.seed(42)
cv.glm(MASS::Pima.tr, dia_select, K = 5)$delta[1]

# 0.1597213

# 13
summary(dia_add)
predict(dia_add, MASS::Pima.te, type = "response") 
pred = ifelse(predict(dia_add, MASS::Pima.te, type = "response") > 0.5, "Yes", "No")
pred
mean(pred != MASS::Pima.te$type)
# 0.1988


# 14
# Report the sensitivity of this classifier in the test dataset
pred = ifelse(predict(dia_add, MASS::Pima.te, type = "response") > 0.5, "Yes", "No")
make_conf_mat = function(predicted, actual){
  table(predicted = predicted, actual = actual)
}
# note here actual = test!!!!!!
conf_mat = make_conf_mat(predicted = pred, actual =  MASS::Pima.te$type)
conf_mat

get_sens = function(conf_mat){
  conf_mat[2, 2] / sum(conf_mat[, 2])
}

get_spec = function(conf_mat){
  conf_mat[1, 1] / sum(conf_mat[, 1])
}

get_sens(conf_mat)
# 0.6055

# 15
pred_30 = ifelse(predict(dia_add, MASS::Pima.te, type = "response") > 0.3, "Yes", "No")
conf_mat_30 = make_conf_mat(predicted = pred_30, actual =  MASS::Pima.te$type)
conf_mat_30
get_sens(conf_mat_30)
# 0.7982
