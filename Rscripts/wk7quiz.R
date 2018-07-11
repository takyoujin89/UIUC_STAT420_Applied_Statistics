# Practice Quiz
# 1
?ToothGrowth
View(ToothGrowth)
str(ToothGrowth)

(dose_num = lm(len ~ dose * supp, data = ToothGrowth))

# obtain an estimate of the change in mean tooth length for a dose increase of 1 milligram per day
# when the supplement type is orange juice.
# beta_1
coef(dose_num)[2]

is.factor(ToothGrowth$supp)
levels(ToothGrowth$supp)
as.numeric(ToothGrowth$supp)
ToothGrowth$supp
coef(dose_num)
# 7.811429

# 2. an estimate of the change in mean tooth length for an dose increase of 1 milligram per day
# when the supplement type is ascorbic acid.
# beta_1 + beta_3
coef(dose_num)[2] + coef(dose_num)[4]
# 11.71571 

# 3.
dose_add = lm(len ~ dose + supp, data = ToothGrowth)
anova(dose_add, dose_num)[2, "Pr(>F)"]
# 0.02463136


# 4.
is.factor(ToothGrowth$dose)
ToothGrowth$dose = as.factor(ToothGrowth$dose)
str(ToothGrowth)

as.numeric(ToothGrowth$dose)

dose_additive = lm(len ~ dose, data = ToothGrowth)
coef(dose_additive)[2] - coef(dose_additive)[3]
# -6.365 
dose_add2 = lm(len ~ dose + supp, data = ToothGrowth)
coef(dose_add2)


# 5.
dose_add3 = lm(len ~ 0 + dose + supp, data = ToothGrowth)
coef(dose_add3)[3]
# 27.95 
# or done with model in q4
coef(dose_add2)[1] + coef(dose_add2)[3]


# Quiz
# 1
# model 1 simple only Bwt
model_sim = lm(Hwt ~ Bwt, data = cats)
coef(model_sim)[2]
# 4.034063

# 2
str(cats)
# model 3 interaction
model_int = lm(Hwt ~ Bwt * Sex, data = cats)
# (Intercept)         Bwt        SexM    Bwt:SexM => Male = 1
# 2.981312    2.636414   -4.165400    1.676265 
# female => x2 = 0
?cats
coef(model_int)[2]
# 2.636414

# 3
# x2 = 1 => y = beta_0 + beta_2 + (beta_1 + beta_3) x1
coef(model_int)[2] + coef(model_int)[4]
# 4.312679

# 4
# model 2 additive
model_add = lm(Hwt ~ Bwt + Sex, data = cats)
coef(model_add)
# Male = 1, x2 = 1 => y = beta_0 + beta_2 + beta_1 x1
# Female = 0, x2 = 0 => y = beta_0 + beta_1 x1
# no difference
# 0


# 5
anova(model_add, model_int)
anova(model_add, model_int)[2, "F"]
# 4.007712

# 6
# int

# 7.
?iris
View(iris)
iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
predict(iris_add, data.frame(Petal.Length = 5.10, Species = "versicolor"))

# 6.695834 

# 8. Create a 90% confidence interval for the difference in mean sepal length between virginicas and setosas for a given petal length. 
# Report the lower bound of this interval.
new_length = data.frame(Petal.Length = c(6, 6), Species = c("virginica", "setosa"))
predict(iris_add, newdata = new_length, interval = "confidence", level = 0.9)[1,2]
predict(iris_add, newdata = new_length, interval = "confidence", level = 0.9)[1,2] - predict(iris_add, newdata = new_length, interval = "confidence", level = 0.9)[2,2]


iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
summary(iris_add)
coef(iris_add)

as.numeric(iris$Species)
# beta_0 + beta_3 - beta_0 = beta_3
confint(iris_add, level = 0.9)[4, 1]
# -2.570345

# 9. 
iris_sim = lm(Sepal.Length ~ Petal.Length, data = iris)
coef(iris_sim)
anova(iris_sim, iris_add)[2, "F"]     
# 34.32311

# 10. 
iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
length(coef(iris_int))
# 6

# 11
new_length = data.frame(Petal.Length = 5.10, Species = "versicolor")
predict(iris_int, newdata = new_length, interval = "prediction", level = 0.99)[3]

# 7.546683

# 12 obtain an estimate of the change in mean sepal length for a petal length increase of 1 unit, 
# for a versicolor.
coef(iris_int)
levels(iris$Species)
# [1] "setosa"     "versicolor" "virginica" 
# (Intercept)                   Petal.Length 
# 4.2131682                      0.5422926 
# Speciesversicolor               Speciesvirginica 
# -1.8056451                     -3.1535091 
# Petal.Length:Speciesversicolor  Petal.Length:Speciesvirginica 
# 0.2859884                      0.4534460 

# y = beta_0 + beta_1 x + beta_2 v2 + beta_3 v3 + gamma_2 x v2 + gamma_3 x v3
# setosa: beta_0 + beta_1 * x
# versicolor: beta_0 + beta_2 + (beta_1 + gamma_2) x
# virginica: beta_0 + beta_3 + (beta_1 + gamma_3) x

coef(iris_int)[2] + coef(iris_int)[5]
# this is answer
#  0.828281  

# 13.
anova(iris_add, iris_int)
# add

# 14.
View(swiss)
coef(Fertility_3)
# x2 = 90. x3 = 20
# y = beta_0 + beta_1 x1 + beta_2 x2 + beta_3 x3 + beta_4 x1 x2 + beta_5 x1 x3 + beta_6 x2 x3 + beta_7 x1 x2 x3
# y = beta_0 + (beta_1 + beta_4 x2 + beta_5 x3 + beta_7 x2 x3) x1 + beta_2 x2 +  beta_3 x3 + beta_6 x2 x3
coef(Fertility_3)[2] + coef(Fertility_3)[5] * 90 + coef(Fertility_3)[6] * 20 + coef(Fertility_3)[8] * 90 * 20
# -1.180297

# 15.
Fertility_3 = lm(Fertility ~ (Education + Catholic + Infant.Mortality) ^ 3,data = swiss)
Fertility_2 = lm(Fertility ~ (Education + Catholic + Infant.Mortality) ^ 2,data = swiss)
summary(Fertility_2)
summary(Fertility_3)
anova(Fertility_2, Fertility_3)[2, "Pr(>F)"]
#  0.3912921
