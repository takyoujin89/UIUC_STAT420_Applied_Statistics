View(poisons)
library(boot)
po_md = lm(time ~ poison + treat, data = poisons)
?poisons
as.factor(poisons$poison)
is.factor(poisons$poison)
summary(po_md)

is.factor(poisons$treat)
summary(po_md)$coefficients[1,1]
summary(po_md)$coefficients[3,1] + summary(po_md)$coefficients[5,1]
-0.2629167

po_int = lm(time ~ poison * treat, data = poisons)
summary(po_int)
anova(po_md, po_int)
anova(po_md, po_int)[2, "Pr(>F)"]
0.1122506

4.6 + 6.2 * 4 + 7.1 * -5 + 8.1 * 4 * -5
-168.1

shapiro.test(resid(po_md))
shapiro.test(resid(po_md))$p.value < 0.1

6 + 2 * 5 + -3 * 4 
pnorm(3, mean = 0, sd = sqrt(5 + 2 * 4), lower.tail = FALSE)
0.2026903

library(MASS)
bs_md = lm(log(medv) ~ rm + lstat + I(lstat ^ 2), data = Boston)
summary(bs_md)
coef(bs_md)["lstat"]
-0.06916971 

bs_2 = lm(log(medv) ~ age + lstat + I(lstat ^ 2), data = Boston)
summary(bs_2)
?Boston
View(Boston)
exp(predict(bs_2, newdata = data.frame(age = 73, lstat = 12)))
exp(3.016538) 
20.42047

rm(airquality)
airquality = na.omit(airquality)
fit = lm(Ozone ~ ., data = airquality)

sum(hatvalues(fit) > 2 * mean(hatvalues(fit)))
2

airquality = na.omit(airquality)
fit = lm(Ozone ~ ., data = airquality)

cooks.distance(fit) > 4 / length(cooks.distance(fit))
cooks.distance(fit)[cooks.distance(fit) > 4 / length(cooks.distance(fit))]
min(cooks.distance(fit)[cooks.distance(fit) > 4 / length(cooks.distance(fit))])
0.05063964

cd = cooks.distance(fit)
cutoff =  4 / nrow(airquality)
min(cd[cd > cutoff])

n = length(resid(fit))
n
nrow(airquality)
air_select = step(fit, k = log(n), trace = 0)
summary(air_select)
sum(resid(air_select) ^ 2)
48002.79


air_int = lm(Ozone ~ . ^ 2, data = airquality)
coef(air_int)
air_ba = step(air_int, trace = 0)
summary(air_ba)
summary(air_ba)$adj.r.squared
0.6716163

library(MASS)
?birthwt
birth_md = glm(low ~ age + lwt + smoke, data = birthwt, family = "binomial")
is.factor(birthwt$smoke)
summary(birth_md)
coef(birth_md)["smoke"]
0.6707637 

summary(birth_md)$coefficients[3, 4]
0.04785919

predict(birth_md, newdata = data.frame(age = 25, lwt = 140, smoke = 0), type = "response")
0.2131511 

birth_null = glm(low ~ age, data =birthwt, family = "binomial")
anova(birth_null, birth_md, test = "LRT")
anova(birth_null, birth_md, test = "LRT")[2, "Pr(>Chi)"]
0.01092936

poison_md = lm(time ~ poison + treat, data = poisons)
summary(poison_md)$coefficients

levels(poisons$poison)
levels(poisons$treat)
coef(poison_md)
