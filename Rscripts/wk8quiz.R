# Practice Quiz
# 1
x = 3
y = 5 - 2 * x + rnorm(n, mean = 0, sd = sqrt(0.75)) 
pnorm(1, mean = -1, sd = sqrt(0.75), lower.tail = FALSE)
# 0.01046067

# 2
quiz_data
quiz_model = lm(y ~ x, data = quiz_data)
cooks.distance(quiz_model)[which.max(hatvalues(quiz_model))]
# 1.966891 

# 3
quiz_model = lm(y ~ x, data = quiz_data)
shapiro.test(resid(quiz_model))
# 0.004584

# 4

quiz_log = lm(log(y) ~ x + I(x ^ 2), data = quiz_data)
shapiro.test(resid(quiz_log))
# 0.4022
# fail to reject, normal

# 5
quiz_log = lm(log(y) ~ x + I(x ^ 2), data = quiz_data)
rss = sum((exp(fitted(quiz_log)) - quiz_data$y) ^ 2)
rss / 1000000000


# Quiz
# 1
model1 = lm(y ~ x, data = data_1)
model2 = lm(y ~ x, data = data_2)
par(mfrow = c(1, 2))
qqnorm(resid(model1))
qqline(resid(model1))
qqnorm(resid(model2))
qqline(resid(model2))
# data_2

# 2
par(mfrow = c(1, 2))
plot(fitted(model1), resid(model1))
abline(h = 0)
plot(fitted(model2), resid(model2))
abline(h = 0)
#data_1

# 3
pnorm(-12, mean = -10, sd = 3)
# 0.2524925

# 4
View(LifeCycleSavings)
life_model = lm(sr ~ ., data = LifeCycleSavings)
mean(abs(rstandard(life_model)) < 2)
sum(abs(rstandard(life_model)) < 2)
# 0.96

# 5
which.max(abs(rstandard(life_model)))
# Zambia           

# 6
sum(hatvalues(life_model) > 2 * mean(hatvalues(life_model)))
# 4

# 7
which.max(hatvalues(life_model))
# Libya

# 8
cooks.distance(life_model)[which.max(cooks.distance(life_model))]
# 0.2680704  

# 9
cd_life = cooks.distance(life_model)
large_cd_life = cd_life > 4 / length(cd_life)
LifeCycleSavings[large_cd_life,]

## notice here, subset is <= !!!
life_fix = lm(sr ~ ., data = LifeCycleSavings,
              subset = cd_life <= 4 / length(cd_life))
coef(life_model)
coef(life_fix)
sum(coef(life_fix))
summary(life_fix)
# 19.63769


# 10.
View(airquality)
airquality = subset(airquality, airquality$Ozone != "NA")
airquality = subset(airquality, airquality$Solar.R != "NA")
air_model = lm(Ozone ~ Temp + I(Temp ^ 2), data = airquality)
summary(air_model)$coefficients[3, 4]

#  0.0004941148

# 11
air_model_q = lm(Ozone ~ poly(Temp, degree = 4, raw = TRUE), data = airquality)
summary(air_model_q)
anova(air_model, air_model_q)
anova(air_model, air_model_q)[2, 6]
# 0.02436082

# 12
# reload the default built in dataset 
rm(airquality)
View(airquality)
airquality = na.omit(airquality)
shapiro.test(resid(air_model_q))
air_model_q
# 5.009e-11
# reject, suspect

# 13
air_model_log = lm(log(Ozone) ~ Temp, data = airquality)
shapiro.test(resid(air_model_log))
# 0.04867
# fail to reject, good

# 14
exp(predict(air_model_log, newdata = data.frame(Temp = 84), interval = "prediction", level = 0.9)[3])
# 122.1224

# 15
fitted(air_model_log)[fitted(air_model_log) < 3.5]

# you are not looking for variance of "residual of  the fitted values".

# you are looking for variance of "residual for observations with certain fitted values "

# this is wrong, since it's based the log model to calculate sample variance of residuals for observations with a fitted value less than 3.5
var(exp(fitted(air_model_log)[fitted(air_model_log) < 3.5]) - airquality$Ozone)
var(exp(fitted(air_model_log)[fitted(air_model_log) > 3.5]) - airquality$Ozone)
var(exp(fitted(air_model_log)[fitted(air_model_log) < 3.5]) - airquality$Ozone) / var(exp(fitted(air_model_log)[fitted(air_model_log) > 3.5]) - airquality$Ozone)
# 0.7461457

# this is correct
var(resid(air_model_log)[fitted(air_model_log) < 3.5]) / var(resid(air_model_log)[fitted(air_model_log) > 3.5])
# 1.353182
