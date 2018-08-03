# practice
# 1
# Regressing the response (hipcenter) against all of the predictors except the predictor of interest (HtShoes).
# Regressing the predictor of interest (HtShoes) against the other predictors (Age, Arm, and Ht).
quiz_data
cor(resid(lm(y ~ . - x1, data = quiz_data)), resid(lm(x1 ~ . - y, data = quiz_data)))
# 0.2024066

# 2 vif = 1 / 1 - Rj2, Rj2 =  multiple R-Squared for the regression of xj on each of the other predictors
quiz_lin = lm(y ~ ., data = quiz_data)
vif(quiz_lin)["x5"]

rj2 = summary(lm(x5 ~ . - y, data = quiz_data))$r.squared
1/ (1 - rj2) 
#  39.87626


#3
mod_all = lm(y ~ ., data = quiz_data)
mod_3 = lm(y ~ x1 + x2 + x3, data = quiz_data)
extractAIC(mod_all)
extractAIC(mod_3)
# [1] 7.000000 2.419733
# [1] 4.000000 4.377553
# calcualte RSS of the better model

sum(resid(mod_all) ^ 2)
deviance(mod_all)
# 39.66296

# 4
mod_124 = lm(y ~ x1 + x2 + x4, data = quiz_data)
mod_3456 = lm(y ~ x3 + x4 + x5 + x6, data = quiz_data)
summary(mod_124)$adj.r.squared
summary(mod_3456)$adj.r.squared

# 0.1390175

# 5
start_mod = lm(y ~ ., data = quiz_data)
back_aic = step(start_mod, direction = "backward")
calc_loocv_rmse = function(model){
  sqrt(mean((resid(model)/ (1 - hatvalues(model))) ^ 2))
}

calc_loocv_rmse(back_aic)
# 0.9785782

# quiz
# 1
View(mtcars)
mod_lin = lm(mpg ~ ., data = mtcars)
library(faraway)
vif(mod_lin)
str(vif(mod_lin))
which.max(vif(mod_lin))
vif(mod_lin)[[which.max(vif(mod_lin))]]
# 21.62024

# 2
summary(mod_lin)$adj.r.squared
# 0.8066423

# 3
calc_loocv_rmse(mod_lin)
# 3.490209

# 4
car_back_aic = step(mod_lin, direction = "backward")
# mpg ~ wt + qsec + am          

# 5
calc_loocv_rmse(car_back_aic)
# 2.688538

# 6
vif(car_back_aic)
vif(car_back_aic)[[which.max(vif(car_back_aic))]]
# 2.541437

# 7
# selected model better predicting and no collinearity

# 8
mod_0 = lm(mpg ~ 1, data = mtcars)
car_for_bic = step(mod_0, scope = mpg ~  cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, direction = "forward", k = log(n))
# mpg ~ wt + cyl

# 9
calc_loocv_rmse(car_for_bic)
# 2.715962

# 10
View(LifeCycleSavings)
cor(resid(lm(sr ~. - ddpi, data = LifeCycleSavings)), resid(lm(ddpi ~ . - sr, data = LifeCycleSavings)))
#  0.2972201

# 11
# . ^ 2 will automatically consider all first-order terms, as well as all two-way interactions
mod_2nd = lm(sr ~ . ^ 2, data = LifeCycleSavings)
mod_2nd = lm(sr ~ (pop15 + pop75 + dpi + ddpi) ^ 2, data = LifeCycleSavings)
summary(mod_2nd)$adj.r.squared
# 0.261233

# 12
life_back_bic = step(mod_2nd, direction = "backward", k = log(n))
# sr ~ dpi + ddpi + dpi:ddpi

# 13
life_back_aic = step(mod_2nd, direction = "backward")
# sr ~ pop15 + dpi + ddpi + dpi:ddpi

# 14
calc_loocv_rmse(life_back_aic)
# 3.833628
calc_loocv_rmse(mod_2nd)


# 15
# for adjust r squared, we need it the bigger the better
life_add = lm(sr ~ ., data = LifeCycleSavings)
summary(life_add)$adj.r.squared
summary(life_back_aic)$adj.r.squared
summary(mod_2nd)$adj.r.squared
#  0.3188961


