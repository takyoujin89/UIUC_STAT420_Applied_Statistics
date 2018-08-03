# Week1
# Practice
x = 1:100
sum(log(x))
363.7394

set.seed(42)
a_vector = rpois(250, lambda = 6)
sum(a_vector >= 5)
181

x = 1:100
i = seq(1, length(x), 2)
y1 = x[i] + 5 
y2 = x[-i] - 10
y = c(y1, y2)
c(y1, y2)
sd(y)
29.8481

quiz_list = list(
  x = c(1, 2),
  y = "Hello Quiz Taker",
  z = "z"
)

quiz_list$z
quiz_list[3] #  returns a list contain the 3rd element
quiz_list[[3]] #  returns the 3rd element of the list, in this case, a vector

hist(rchisq(1500, df = 5), breaks = 20)

#quiz
1
library(MASS)
View(Melanoma)
?Melanoma
sum(Melanoma$status[Melanoma$status == 1])
57

2
mean(Melanoma$age[Melanoma$status == 2])
50.00746

3
?mammals
mammals[which.max(mammals$brain / mammals$body),]
#body brain
#Ground squirrel 0.101     4


4
?iris
boxplot(hwy ~ drv, data = mpg)
par(mfrow = c(1, 4))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)
#Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
boxplot(iris[, -5])

sd(iris$Petal.Length)
1.765298

5
set.seed(42)
z = list(
  round(rnorm(n = 25, 0, 5), 2),
  c(1, 1, 2, 3, 5, 8),
  sample(30)
)
(min(z[[1]]) + max(z[[2]]) + mean(z[[3]]))
10.22

7
?airquality
View(airquality)
mean(airquality$Wind[airquality$Month == 5])
mean(subset(airquality, Month == 5)$Wind)

11.62258

8
View(airquality)
airquality = na.omit(airquality)
?na.omit
mean(airquality$Ozone)
mean(airquality$Ozone, na.rm = TRUE)
42.0991

10
set.seed(1337)
x = rnorm(10000)
x
mean(x>2)
mean(abs(x)>2)
0.0444

11
set.seed(42)
x = rnorm(100, mean = 0, sd = 10)
mean(f(input = x)) - f()

f = function(input = 42){
  ifelse(input < 0, 0, input)
}

f()
-37.70725

12
set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)
x0 = rep(1, 30)
x1 = seq(1, 30) ^ 2
mean(y)
320.2353

13
X = cbind(x0,x1)
sum(X[17, ] + X[19,])
sum(X[c(17, 19),])
652

14
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
sum(beta_hat)
6.427899
X
beta_hat

15
y_hat = X %*% beta_hat
sum((y - y_hat) ^ 2)
42.67698


# week2
# practice
pnorm(q = 4, mean = 5, sd = 3, lower.tail = FALSE)
0.6305587

y = -3 + 2.5 * 5
y
9.5

2

tree_md = lm(Girth ~ Height, data = trees)
coef(tree_md)
0.2557471

summary(tree_md)$r.squared
0.2696518

# quiz
y = 10 + 5 * x  
pnorm(q = -4, mean = 0, sd = 4)
0.1586553

x = -1
y 
pnorm(q = -2, mean = 0, sd = 4, lower.tail = FALSE)
0.6914625


x = -2
y
pnorm(q = 3, mean = 0, sd = 4, lower.tail = FALSE)
0.2266274

View(faithful)
faithful_md = lm(eruptions ~ waiting, data = faithful)
coef(faithful_md)[1]
-1.874016

coef(faithful_md)[2]
0.07562795 

predict(faithful_md, newdata = data.frame(waiting = 80))
4.17622

predict(faithful_md, newdata = data.frame(waiting = 120))
7.201338 

range(faithful$waiting)
80

n = nrow(faithful)
n
# calcualte RSS same as 
sum(resid(faithful_md) ^ 2)
(summary(faithful_md)$sigma) ^ 2 * (n-2)
66.56178

summary(faithful_md)$r.squared
0.8114608

sd(resid(faithful_md))
0.495596


# week3
pt(q = 1.3, df = 7, lower.tail = FALSE)
0.1173839

qt(p = 0.025, df = 9, lower.tail = FALSE)
2.262157

summary(tree_md)$coefficients[2, 4]
0.002757815

confint(tree_md, level = 0.9)[2,2] - confint(tree_md, level = 0.9)[2,1]
0.2656018

predict(tree_md, newdata = data.frame(Height = 79), interval = "confidence", level = 0.95)
#fit     lwr      upr
#1 14.01563 12.9048 15.12646

pt(q = 2.1, df = 5, lower.tail = FALSE) * 2
0.04487662
0.08975325



1 - (1 - 0.9) / 2
qt(0.95, df = 10 - 2)
1.859548

Sxx = 1.5
beta_1_sigma = sqrt(4 / Sxx)
beta_1_sigma
pnorm(q = 4.2, mean = 4, sd = beta_1_sigma, lower.tail = FALSE)
0.4512616

summary(faithful_md)$coefficients
summary(faithful_md)$coefficients[2, 2]
0.002218541

summary(faithful_md)$coefficients[1, 3]
-11.70212

summary(faithful_md)$coefficients[2, 3]
34.08904

confint(faithful_md, level = 0.9)
confint(faithful_md, level = 0.9)[1, 2]
-1.609697

#length of the margin of this interval
interval = confint(faithful_md, level = 0.95)[2, 2] - confint(faithful_md, level = 0.95)[2, 1]
margin = interval / 2
margin
0.00436784


predict(faithful_md, newdata = data.frame(waiting = 81), interval = "confidence", level = 0.9)
predict(faithful_md, newdata = data.frame(waiting = 81), interval = "confidence", level = 0.9)[,"lwr"]
4.189899

predict(faithful_md, newdata = data.frame(waiting = 72), interval = "prediction", level = 0.99)
predict(faithful_md, newdata = data.frame(waiting = 72), interval = "prediction", level = 0.99)[3]
4.861612

faithful_null = lm(eruptions ~ 1, data = faithful)
anova(faithful_null, faithful_md)

anova(faithful_null, faithful_md)[2, "Res.Df"]
anova(faithful_null, faithful_md)[2, "Df"]

# week4
# practice
pf(q = 2.7, df1 = 3, df2 = 5, lower.tail = FALSE)
0.1561342

ll_md = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
confint(ll_md, level = 0.9)
confint(ll_md, level = 0.9)[2, 1]
0.05579357

summary(ll_md)$coefficients
summary(ll_md)$coefficients[3, 2]
0.1859156

summary(ll_md)$coefficients[4, 4]
0.0970466

summary(ll_md)$fstatistic["value"]
238.5757 

n = 30
p = 9 + 1
pf(q = 2.4, df1 = p - 1, df2 = n - p, lower.tail = FALSE)
0.04943057

n = 20
p = 5 + 1
# for SLR, Follow ~ t n-2, MLR: ~ t n - p 
pt(q = -1.3, df = n - p) * 2
0.2145976

set.seed(42)
x_values = data.frame(
  x0 = rep(1, 15),
  x1 = runif(15),
  x2 = runif(15),
  x3 = runif(15))


sigma = 3
beta_0 = 3
beta_1 = 2
beta_2 = 0.5
beta_3 = 5


x0 = rep(1, 15)
x1 = runif(15)
x2 = runif(15)
x3 = runif(15)
X
X = cbind(x0, x1, x2, x3)
# or 
X = as.matrix(cbind(rep(1, 15), x_values))

C = solve(t(X) %*% X)


sqrt(sigma ^ 2 * C[3, 3])
2.47399


ss_md = lm(Fertility ~ ., data = swiss)
?swiss
summary(ss_md)
newdata = data.frame(Agriculture = 54, Examination = 23, Education = 13, Catholic = 60, Infant.Mortality = 24)
predict(ss_md, newdata = newdata)
72.46069 

confint(ss_md, level = 0.99)[5, 2]
0.1993532

summary(ss_md)$coefficients
summary(ss_md)$coefficients[3, 4]
0.3154617

predict(ss_md, newdata = data.frame(Agriculture = 40, Examination = 28, Education = 10, Catholic = 42, Infant.Mortality = 27), interval = "confidence", level = 0.95)[,"lwr"]
69.4446

predict(ss_md, newdata = data.frame(Agriculture = 40, Examination = 28, Education = 10, Catholic = 42, Infant.Mortality = 27), interval = "prediction", level = 0.95)[, "lwr"]
60.96392

summary(ss_md)$fstatistic["value"]
19.76106

ss_null = lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss)
anova(ss_null, ss_md)
anova(ss_null, ss_md)[2, "Pr(>F)"]
0.05628314

# midterm 
set.seed(42)
pnorm(q = 4, mean = 3, sd = sqrt(11), lower.tail = FALSE)
0.3815123

set.seed(42)
l = rnorm(n = 350, mean = 3, sd = sqrt(11))
mean(l > 4)
          
(some_fun() + some_fun(arg1 = 1:4, arg2 = 4)) / some_fun(arg1 = 1:9, arg2 = 2)
some_fun = function(arg1 = 1, arg2 = 2){
  mean(arg1 / arg2)
}
0.45

omd = lm(circumference ~ age, data = Orange)
summary(omd)$r.squared
0.8345167

range(Orange$age)
predict(omd, newdata = data.frame(age = 400))
60.10778 

confint(omd, level = 0.9)
confint(omd, level = 0.9)[2, 1]

predict(omd, newdata = data.frame(age = 250), interval = "confidence", level = 0.9)[,"lwr"]
32.48418

predict(omd, newdata = data.frame(age = 400), interval = "prediction", level = 0.99)[,"upr"]
126.9615

beta_1_hat = summary(omd)$coefficients["age", "Estimate"]
beta_1_se = summary(omd)$coefficients[2, 2]
beta_1_hat
beta_1_se

t = (beta_1_hat - 0.123) / beta_1_se
n = length(resid(omd))
pt(abs(t), df = n - 2, lower.tail = FALSE) * 2
0.05837492

pnorm(q = 1, mean = 0, sd = 2)
0.6914625

2 + 1.5 * 1 - 2.1 * 2 + 3.2 * 3 
pnorm(q = 11 - 8.9, mean = 0, sd = 2, lower.tail = FALSE)
0.1468591

set.seed(420)
x_values = data.frame(
  x1 = rnorm(10),
  x2 = runif(10),
  x3 = runif(10),
  x4 = runif(10)
)
X = as.matrix(cbind(rep(1, 10), x_values))
X
C = solve(t(X) %*% X)
beta_2_hat_sigma = sqrt(9 * C[3, 3])
beta_2_hat_sigma
4.45513

1 - (1 - 0.9) / 2

n = 15
p = 4 + 1
crit = qt(0.95, df = n - p) 
crit
1.812461

library(MASS)
?Boston
bs_md = lm(medv ~ rm + nox, data = Boston)
summary(bs_md)$r.squared
0.535438

bs_md2 = lm(medv ~ lstat + rm + crim + tax + nox, data = Boston)
summary(bs_md2)
summary(bs_md2)$coefficients["tax", 4]
0.003344395

bs_mdn = lm(medv ~ lstat + rm, data = Boston)
anova(bs_mdn, bs_md2)
anova(bs_mdn, bs_md2)[2, "F"]
md2
anova(bs_mdn, bs_md2)[2, "RSS"]


# week7
# practice
?ToothGrowth
View(ToothGrowth)
is.factor(ToothGrowth$supp)
tg_md = lm(len ~ dose * supp, data = ToothGrowth)
summary(tg_md)
coef(tg_md)
coef(tg_md)["dose"]
7.811429

coef(tg_md)[2] + coef(tg_md)[4]
11.71571 

tg_add = lm(len ~ dose + supp, data = ToothGrowth)
anova(tg_add, tg_md)
anova(tg_add, tg_md)[2, "Pr(>F)"]
summary(tg_md)$coefficients[4, 4]
0.02463136

is.factor(ToothGrowth$dose)
ToothGrowth$dose = as.factor(ToothGrowth$dose)
levels(ToothGrowth$dose)
tg_dd = lm(len ~ dose + supp, data = ToothGrowth)
summary(tg_dd)
coef(tg_dd)[2] - coef(tg_dd)[3]
-6.365

fit = lm(len ~ as.factor(dose) + supp, data = ToothGrowth)
tg_dd2 = lm(len ~ 0 + dose + supp, data = ToothGrowth)
summary(tg_dd2)
coef(tg_dd2)[3]
27.95 

# quiz
library(MASS)
?cats
cat_sim = lm(Hwt ~ Bwt, data = cats)
is.factor(cats$Sex)
cat_add = lm(Hwt ~ Bwt + Sex, data = cats)
cat_int = lm(Hwt ~ Bwt * Sex, data = cats)

summary(cat_sim)
coef(cat_sim)[2]
4.034063 

summary(cat_int)
coef(cat_int)[2]
2.636414

coef(cat_int)[2] + coef(cat_int)[4]
coef(cat_int)[4]
4.312679 

coef(cat_add)
0

anova(cat_add, cat_int)
anova(cat_add, cat_int)[2, "F"]
4.007712

int

iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
levels(iris$Species)
predict(iris_add, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"))
6.695834 

predict(iris_add, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"))
confint(iris_add, level = 0.9)[4, 1]
-2.570345

iris_no = lm(Sepal.Length ~ Petal.Length, data = iris)
anova(iris_no, iris_add)[2, "F"]
34.32311

iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
length(coef(iris_int))
6

predict(iris_int, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"), interval = "prediction", level = 0.99)
7.546683

summary(iris_int)
summary(iris_int)$coefficients[2, 1] + summary(iris_int)$coefficients[5, 1]
0.828281

anova(iris_add, iris_int)
add

ss_md_3 = lm(Fertility ~ (Education + Catholic + Infant.Mortality) ^ 3, data = swiss)
cf = summary(ss_md_3)$coefficients
cf[2, 1] + cf[5, 1] * 90 + cf[6, 1] * 20 + cf[8, 1] * 90 * 20
-1.180297

cf[8, 4]
0.3912921

# week8
# practice
x = 3
y = 5 - 2 * x
y
pnorm(q = 2, mean = 0, sd = sqrt(0.75),lower.tail = FALSE)
0.01046067

gen_data = function(sample_size = 20, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = exp(2 + 3 * x + 0.35 * x ^ 2 + rnorm(n = sample_size, sd = 3))
  data.frame(x = x, y = y)
}

quiz_data = gen_data()
quiz_md = lm(y ~ x, data = quiz_data)

hatvalues(quiz_md)
which.max(hatvalues(quiz_md))

cooks.distance(quiz_md)
which.max(cooks.distance(quiz_md))
cooks.distance(quiz_md)[which.max(hatvalues(quiz_md))]
1.966891 

shapiro.test(resid(quiz_md))
names(shapiro.test(resid(quiz_md)))
shapiro.test(resid(quiz_md))$p.value
0.004583584

quiz_log = lm(log(y) ~ x + I(x ^ 2), data = quiz_data)
shapiro.test(resid(quiz_log))$p.value
# fail to reject h0, not suspect

sum((quiz_data$y - exp(fitted(quiz_log))) ^ 2) / 1000000000
42.27957

# quiz
gen_data_1 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rnorm(n = sample_size)
  data.frame(x = x, y = y)
}

gen_data_2 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rt(n = sample_size, df = 2)
  data.frame(x = x, y = y)
}

data_1 = gen_data_1()
data_2 = gen_data_2()

md1 = lm(y ~ x, data = data_1)
md2 = lm(y ~ x, data = data_2)
qqnorm(resid(md1))
qqline(resid(md1))

qqnorm(resid(md2))
qqline(resid(md2))

shapiro.test(resid(md1))
shapiro.test(resid(md2))
# md2

gen_data_2 = function(sample_size = 100, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rnorm(n = sample_size)
  data.frame(x = x, y = y)
}

gen_data_1 = function(sample_size = 100, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = -3, max = 0)
  y = 2 + 3 * x + sqrt(abs(x * rnorm(n = sample_size)))
  data.frame(x = x, y = y)
}

data_1 = gen_data_1()
data_2 = gen_data_2()

md1 = lm(y ~ x, data = data_1)
md2 = lm(y ~ x, data = data_2)


plot(fitted(md1), resid(md1))
abline(h = 0)
plot(fitted(md2), resid(md2))
abline(h = 0)

library(lmtest)
bptest(md1)
bptest(md2)
# md1

pnorm(-2, mean = 0, sd = 3)
pnorm(-12, mean = -10, sd = 3)
0.2524925

life_md = lm(sr ~ ., data = LifeCycleSavings)
rstandard(life_md)[abs(rstandard(life_md)) < 2] 
mean(abs(rstandard(life_md)) < 2)
0.96

which.max(rstandard(life_md))
Zambia 

sum(hatvalues(life_md) > 2 * mean(hatvalues(life_md)))
4

which.max(hatvalues(life_md))
Libya

cooks.distance(life_md)
cooks.distance(life_md)[which.max(cooks.distance(life_md))]
max(cooks.distance(life_md))
0.2680704 

cd_life = cooks.distance(life_md)
cd_life > 4 / length(cd_life)

life_fix = lm(sr ~ ., data = LifeCycleSavings, 
              subset = cd_life <= 4 / length(cd_life))
coef(life_fix)
sum(coef(life_fix))
19.63769

airquality = na.omit(airquality)
air_md = lm(Ozone ~ Temp + I(Temp ^ 2), data = airquality)
air_single = lm(Ozone ~ Temp, data = airquality)
anova(air_single, air_md)[2, "Pr(>F)"]
0.0004941148

air_4 = lm(Ozone ~ poly(Temp, 4, raw = TRUE), data = airquality)
summary(air_4)
anova(air_md, air_4)[2, "Pr(>F)"]
0.02436082

shapiro.test(resid(air_4))
#reject H0, suspect

air_log = lm(log(Ozone) ~ Temp, data = airquality)
shapiro.test(resid(air_log))
# fail to reject H0, not suspect

exp(predict(air_log, newdata = data.frame(Temp = 84), interval = "prediction", level =0.9))[,"upr"]
122.1224

exp(predict(air_log, newdata = data.frame(Temp = 84), interval = "prediction", level =0.9)[,"upr"])


airquality$Ozone[fitted(air_log) < 3.5] 

fitted(air_log)[fitted(air_log) < 3.5] - airquality$Ozone[fitted(air_log) < 3.5]

length(fitted(air_log)[fitted(air_log) < 3.5])
length(airquality$Ozone)

var(resid(air_log)[fitted(air_log) < 3.5] )/ var(resid(air_log)[fitted(air_log) > 3.5] )
var(resid(air_log)[fitted(air_log) < 3.5] )/ var(resid(air_log)[fitted(air_log) > 3.5] )
1.353182

#week9
# practice
gen_data = function() {
  n = 50
  x1 = runif(n)
  x2 = runif(n)
  x3 = runif(n)
  x4 = runif(n)
  x5 = x4 + rnorm(n, sd = 0.05)
  x6 = runif(n)
  y = x1 + x3 + x5 + rnorm(n)
  data.frame(y, x1, x2, x3, x4, x5, x6)
}
set.seed(42)
quiz_data = gen_data()

cor(resid(lm(y ~ .- x1, data = quiz_data)), resid(lm(x1 ~ . - y, data = quiz_data)))
0.2024066

quiz_md = lm(y ~ ., data = quiz_data)
vif(quiz_md)["x5"]
39.87626 

quiz_123 = lm(y ~ x1 + x2 + x3, data = quiz_data)
summary(quiz_123)

names(quiz_123)
extractAIC(quiz_123)
extractAIC(quiz_md)
sum(resid(quiz_md) ^ 2)
39.66296
AIC(quiz_123)

quiz_124 = lm(y ~ x1 + x2 + x3, data = quiz_data)
quiz_3456 = lm(y ~ x3 + x4 + x5 + x6, data = quiz_data)
summary(quiz_124)$adj.r.squared
summary(quiz_3456)$adj.r.squared
0.1390175

summary(quiz_md)
quiz_select = step(quiz_md, direction = "backward", trace = 0)
quiz_select
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
calc_loocv_rmse(quiz_select)

0.9785782

# quiz
car_add = lm(mpg ~ ., data = mtcars)
max(vif(car_add))
21.62024

summary(car_add)$adj.r.squared
0.8066423

calc_loocv_rmse(car_add)
3.490209

car_select = step(car_add, direction = "backward", trace = 0)
summary(car_select)
#wt qsec am

calc_loocv_rmse(car_select)
2.688538

max(vif(car_select))
2.541437

n = length(resid(car_add))
car_no = lm(mpg ~ 1, data = mtcars)
car_fb = step(car_no, scope = mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, direction = "forward", k = log(n))
summary(car_fb)

summary(car_add)
# wt cyl

calc_loocv_rmse(car_fb)
2.715962

cor(resid(lm(sr ~ . - ddpi, data = LifeCycleSavings)), resid(lm(ddpi ~ . - sr, data = LifeCycleSavings)))
0.2972201

life_int = lm(sr ~ . ^ 2, data = LifeCycleSavings)
summary(life_int)$adj.r.squared
0.261233

n = length(resid(life_int))
life_bb = step(life_int, direction = "backward", k = log(n), trace = 0)
summary(life_bb)
# pop15, dpi, ddpi, dpi:ddpi

life_ba = step(life_int, direction = "backward", trace = 0)
summary(life_ba)
# pop15, dpi, ddpi, dpi:ddpi

calc_loocv_rmse(life_int)
calc_loocv_rmse(life_ba)
calc_loocv_rmse(life_md)
summary(life_md)
3.833628

summary(life_int)$adj.r.squared
summary(life_ba)$adj.r.squared
summary(life_md)$adj.r.squared
0.3188961

# week10
# practice
eta = 2 + -1 * 1 + -1 * 0
eta
p = 1 / (1 + exp(-eta))
p
0.7310586

make_sim_data = function(n = 100) {
  x1 = rnorm(n = n)
  x2 = rnorm(n = n, sd = 2)
  x3 = rnorm(n = n, sd = 3)
  x4 = rnorm(n = n)
  x5 = rnorm(n = n)
  x6 = rnorm(n = n)
  x7 = rnorm(n = n)
  eta = -1 + 0.75 * x2 + 2.5 * x6
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = n, 1, prob = p)
  data.frame(y, x1, x2, x3, x4, x5, x6, x7)
}

set.seed(1)
quiz_data = make_sim_data()

quiz_md = glm(y ~ ., data = quiz_data, family = "binomial")
summary(quiz_md)
coef(quiz_md)["x2"]
0.8839991 

summary(quiz_md)$coefficients[4, 4]
0.2395128

n = length(resid(quiz_md))
quiz_select = step(quiz_md, direction = "backward", k = log(n), trace = 0)
summary(quiz_select)
anova(quiz_select, quiz_md, test = "LRT")[2, "Pr(>Chi)"]
0.7695132

library(boot)
set.seed(1)
cv.glm(quiz_data, quiz_md, K = 5)$delta[1]

# quiz
beta_0 = -3
beta_1 = 1
beta_2 = 2
beta_3 = 3

x1 = -1
x2 = 0.5
x3 = 0.25

eta = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3
eta
p = 1 / (1 + exp(-eta))
1 - p
0.9046505

car_glm = glm(am ~ mpg + hp + qsec, data = mtcars, family = "binomial")
coef(car_glm)["qsec"]
-4.040952 

coef(car_glm)["mpg"]
2.29643 

predict(car_glm, newdata = data.frame(mpg = 19, hp = 150, qsec = 19))
predict(car_glm, newdata = data.frame(mpg = 19, hp = 150, qsec = 19), type = "link")
-8.338686 

predict(car_glm, newdata = data.frame(mpg = 22, hp = 123, qsec = 18), type = "response")
0.916414 

car_null = glm(am ~ 1, data = mtcars, family = "binomial")
anova(car_null, car_glm, test = "LRT")[2, "Deviance"]
35.74953

summary(car_glm)$coefficients[3, 4]
0.8771707

library(MASS)
?Pima.tr
pima_glm = glm(type ~ glu + ped + I(glu ^ 2) + I(ped ^ 2) + glu : ped, data = Pima.tr, family = "binomial")
summary(pima_glm)
coef(pima_glm)["I(ped^2)"]
-0.3595626 

mean(predict(pima_glm, data = Pima.te, type = "response") > 0.8)
mean(predict(pima_glm, newdata = Pima.te, type = "response") > 0.8)
0.07228916

pima_all = glm(type ~ ., data = Pima.tr, family = "binomial")
pima_ba = step(pima_all, direction = "backward", trace = 0)
coef(pima_ba)
length(coef(pima_ba)) - 1
5

pima_big = glm(type ~ . ^ 2, data = Pima.tr, family = "binomial")
summary(pima_big)
pima_ba2 = step(pima_big, trace = 0)
summary(pima_ba2)
deviance(pima_ba2)
162.6924

library(MASS)
library(boot)

# fit the models here

set.seed(42)
cv.glm(Pima.tr,pima_glm, K = 5)$delta[1]
set.seed(42)
cv.glm(Pima.tr, pima_all, K = 5)$delta[1]
set.seed(42)
cv.glm(Pima.tr, pima_ba, K = 5)$delta[1]
set.seed(42)
cv.glm(Pima.tr, pima_big, K = 5)$delta[1]
set.seed(42)
cv.glm(Pima.tr, pima_ba2, K = 5)$delta[1]
0.1597213




Pima.te$type
pima_tst_pred = ifelse(predict(pima_all, Pima.te, type = "response") > 0.5, 
                       "Yes", 
                       "No")
mean(pima_tst_pred!= Pima.te$type)
0.1987952

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

cont_mat = make_conf_mat(predicted = pima_tst_pred, actual = Pima.te$type)
get_sens = function(conf_mat) {
  conf_mat[2, 2] / sum(conf_mat[, 2])
}

get_sens(cont_mat)
0.6055046

pima_tst_pred_30 = ifelse(predict(pima_all, Pima.te, type = "response") > 0.3, 
                       "Yes", 
                       "No")
cont_mat_30 = make_conf_mat(predicted = pima_tst_pred_30, actual = Pima.te$type)
get_sens(cont_mat_30)
0.7981651

### twice
#7
View(ToothGrowth)
as.numeric(ToothGrowth$dose)
tooth_md = lm(len ~ dose + supp + dose : supp, data = ToothGrowth)
summary(tooth_md)$coefficients[2, 1]

coef(tooth_md)["dose"] + coef(tooth_md)["dose:suppVC"]

summary(tooth_md)$coefficients[4, 4]

ToothGrowth$dose = as.factor(ToothGrowth$dose)
tooth_add = lm(len ~ dose + supp, data = ToothGrowth)
summary(tooth_add)
coef(tooth_add)["dose1"] - coef(tooth_add)["dose2"]

tooth_2 = lm(len ~ 0 + dose + supp, data = ToothGrowth)
summary(tooth_2)
coef(tooth_2)["dose2"]


library(MASS)
?cats
cat_sim = lm(Hwt ~ Bwt, data = cats)
cat_add = lm(Hwt ~ Bwt + Sex, data = cats)
cat_int = lm(Hwt ~ Bwt * Sex, data = cats)

summary(cat_sim)
coef(cat_sim)["Bwt"]

summary(cat_int)
coef(cat_int)["Bwt"]

coef(cat_int)["Bwt"] + coef(cat_int)["Bwt:SexM"]

iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
summary(iris_add)

predict(iris_add, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"))

predict(iris_add, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"))

confint(iris_add, level = 0.9)[4, 1]

iris_no = lm(Sepal.Length ~ Petal.Length, data = iris)
anova(iris_no, iris_add)[2, "F"]

iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
length(coef(iris_int)) - 1

predict(iris_int, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor"), interval = "prediction", level = 0.99)[,"upr"]
summary(iris_int)
coef(iris_int)["Petal.Length"] + coef(iris_int)["Petal.Length:Speciesversicolor"]

swiss_md = lm(Fertility ~  (Education + Catholic + Infant.Mortality) ^ 3, data = swiss)
summary(swiss_md)

swiss_md2 = lm(Fertility ~  (Education + Catholic + Infant.Mortality) ^ 2, data = swiss)
anova(swiss_md2, swiss_md)

pnorm(2, mean = 0, sd = sqrt(0.75), lower.tail = FALSE)

gen_data = function(sample_size = 20, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = exp(2 + 3 * x + 0.35 * x ^ 2 + rnorm(n = sample_size, sd = 3))
  data.frame(x = x, y = y) } 



quiz_data = gen_data()
quiz_md = lm(y ~ x, data = quiz_data)
cooks.distance(quiz_md)[which.max(hatvalues(quiz_md))]

quiz_log = lm(log(y) ~ x + I(x ^ 2), data = quiz_data)
shapiro.test(resid(quiz_log))

fitted(quiz_log)
sum((quiz_data$y - exp(fitted(quiz_log))) ^ 2) / 1000000000

quiz_data$y

gen_data_1 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rnorm(n = sample_size)
  data.frame(x = x, y = y)
}
gen_data_2 = function(sample_size = 25, seed = 420) {
  set.seed(seed)
  x = runif(n = sample_size, min = 0, max = 3)
  y = 2 + 3 * x + rt(n = sample_size, df = 2)
  data.frame(x = x, y = y)
}

data_1 = gen_data_1()
data_2 = gen_data_2()
md1 = lm(y ~ x, data = data_1)
md2 = lm(y ~ x, data = data_2)

qqnorm(resid(md1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(md1), col = "dodgerblue", lwd = 2)

qqnorm(resid(md2), main = "Normal Q-Q Plot, fit_2", col = "darkgrey")
qqline(resid(md2), col = "dodgerblue", lwd = 2)
shapiro.test(resid(md1))
shapiro.test(resid(md2))

pnorm(-2, mean = 0, sd = 3)

life_md = lm(sr ~ ., data = LifeCycleSavings)
mean(abs(rstandard(life_md)) < 2)

sum(hatvalues(life_md) > 2 * mean(hatvalues(life_md)))

cd_life = cooks.distance(life_md)
life_fix = lm(sr ~ .,
                    data = LifeCycleSavings,
                    subset = cd_life <= 4 / length(cd_life))
sum(coef(life_fix))

rm(airquality)
airquality = na.omit(airquality)
air_md = lm(Ozone ~ Temp + I(Temp ^ 2), data = airquality)
summary(air_md)

air_4 = lm(Ozone ~ poly(Temp, 4, raw = TRUE), data = airquality)
anova(air_md, air_4)

air_log = lm(log(Ozone) ~ Temp, data = airquality )
exp(predict(air_log, newdata = data.frame(Temp = 84), interval = "prediction", level = 0.9)[,"upr"])

airquality$Ozone[(fitted(air_log) < 3.5)] - exp(fitted(air_log) < 3.5)

var(resid(air_log)[(fitted(air_log) < 3.5)]) / var(resid(air_log)[(fitted(air_log) > 3.5)])

# week9
gen_data = function() {
  n = 50
  x1 = runif(n)
  x2 = runif(n)
  x3 = runif(n)
  x4 = runif(n)
  x5 = x4 + rnorm(n, sd = 0.05)
  x6 = runif(n)
  y = x1 + x3 + x5 + rnorm(n)
  data.frame(y, x1, x2, x3, x4, x5, x6)
}
set.seed(42)
quiz_data = gen_data()

x5_mod = lm(x5 ~ . - y, data = quiz_data)
1 / (1 - summary(x5_mod)$r.squared)

quiz_all = lm(y ~ ., data = quiz_data)
quiz_123 = lm(y ~ x1 + x2 + x3, data = quiz_data)
AIC(quiz_all)
AIC(quiz_123)

sum(resid(quiz_all) ^ 2)

# week10
make_sim_data = function(n = 100) {
  x1 = rnorm(n = n)
  x2 = rnorm(n = n, sd = 2)
  x3 = rnorm(n = n, sd = 3)
  x4 = rnorm(n = n)
  x5 = rnorm(n = n)
  x6 = rnorm(n = n)
  x7 = rnorm(n = n)
  eta = -1 + 0.75 * x2 + 2.5 * x6
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = n, 1, prob = p)
  data.frame(y, x1, x2, x3, x4, x5, x6, x7)
}
set.seed(1)
quiz_data = make_sim_data()

quiz_glm = glm(y ~ ., data = quiz_data, family = "binomial")
coef(quiz_glm)
