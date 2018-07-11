set.seed(42)
r = rnorm(350, mean = 3, sd = sqrt(11))
mean(r > 4)


(some_fun() + some_fun(arg1 = 1:4, arg2 = 4)) / some_fun(arg1 = 1:9, arg2 = 2)

some_fun = function(arg1 = 1, arg2 = 2){
  mean(arg1 / arg2)
}

some_fun()

?iris
iris_md = lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(iris_md)$r.squared

?Orange
View(Orange)
or_md = lm(circumference ~ age, data = Orange)

range(Orange$age)

predict(or_md, newdata = data.frame(age = 400))

confint(or_md, interval = "confidence", level = 0.9)
mean(predict(or_md, parm = "age", level = 0.9))


predict(or_md, newdata = data.frame(age = 250), interval = "confidence", level = 0.9)[2]

confint(or_md, parm = "age", level = 0.9)
confint(or_md, parm = "age")

predict(or_md, newdata = data.frame(age = 500), interval = "prediction", level = 0.95)[2]

#==========================================================
# calcuate p_value of test H0: beta_1 != non-zero
(beta_1_hat - beta_1_1) / (se * sqrt( Sxx))
beta_1_1 = 0.127
x = Orange$age
Sxx = sum((x - mean(x)) ^ 2)                           

summary(or_md)$coefficients
(beta_1_hat = summary(or_md)$coefficients["age", "Estimate"])
(beta_1_se =  summary(or_md)$coefficients["age", "Std. Error"])
n =  nrow(Orange)
n
# note that t = t = (beta_1_hat - beta_1_1) / beta_1_se
# t = (beta_1_hat - beta_1_1) / (se * sqrt( Sxx))
# beta_1_se and se are different, se = summary(model)$sigma
t = (beta_1_hat - beta_1_1) / se
t
pt(abs(t), df = n - 2, lower.tail = FALSE) * 2




orange_model = lm(circumference ~ age, data = Orange)
n = length(resid(orange_model))
n
est = summary(orange_model)$coefficients[2, "Estimate"]
est
se = summary(orange_model)$coefficients[2, "Std. Error"]
se
t = (est - 0.127) / se
t
2 * pt(abs(t), df = n - 2, lower.tail = FALSE)

# 0.682095

?offset


new_omodel = lm(circumference ~ age, data = Orange, offset = 0.127 * age)
summary(new_omodel)$coefficients[2, 4]


x1= 0
x2  = 0
x3 = 0
y = 4 + 1.3 * x1 - 2.4 * x2 + 3.7 * x3
y
pnorm(3, mean = 4, sd = 5)


x1= 1
x2  = 2
x3 = 3
y = 2 + 1.5 * x1 - 2.1 * x2 + 3.2 * x3
y
pnorm(11, mean = 8.9, sd = 2, lower.tail = FALSE)


1 - (1 - 0.9) / 2
p = 5
n = 15

crit = qt(0.95, df = n - p) 
crit

qt(0.05, df = n - p, lower.tail = FALSE)

library(MASS)
?Boston

boston_md = lm(medv ~ rm + nox, data = Boston)
summary(boston_md)$r.squared

b2_md = lm(medv ~ lstat + rm + crim + tax + nox, data = Boston) 
summary(b2_md)$coefficients[5, 4]
summary(b2_md)

boston_null = lm(medv ~ lstat + rm, data = Boston)
anova(boston_null, b2_md)
anova(boston_null, b2_md)[2, 2]


x_values = data.frame(
  x1 = rnorm(10),
  x2 = runif(10),
  x3 = runif(10),
  x4 = runif(10)
)

set.seed(420)
x0 = rep(1, 10)
x1 = rnorm(10)
x2 = runif(10)
x3 = runif(10)
x4 = runif(10)
X = cbind(x0, x1, x2,x3,x4)
C = solve(t(X) %*% X )

(sqrt(3 ^ 2 * C[3,3]))

