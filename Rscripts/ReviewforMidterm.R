# week1
x = 1:100
sum(log(x)) # 363.7394

set.seed(42)
a_vector = rpois(250, lambda = 6)

sum(a_vector>=5) # 181

x = 1:100
y1 = x[x %% 2 == 0] + 5
y2 = x[x %% 2 != 0] - 10
y = y1 + y2
sd(y)  # 58.30952
# better method
c = c(5, -10)
(y = x + c)
sd(y)

library(MASS)
hist(Melanoma$age)
View(Melanoma)
?Melanoma
#===================================================================
sum(Melanoma$status == 1)
table(Melanoma$status)

mean(Melanoma$age)  # 52.46341

?mammals
mammals
mammals(which.max(mammals$brain / mammals$body))
head(mammals)
#===================================================================
mammals[which.max(mammals$brain / mammals$body), c("body", "brain")]
#                  body brain
# Ground squirrel 0.101     4

?iris
head(iris)
#===================================================================
boxplot(iris[,-5])
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length) #1.765298 
sd(iris$Petal.Width)

set.seed(42)
z = list(
  round(rnorm(n = 25, 0, 5), 2),
  c(1, 1, 2, 3, 5, 8),
  sample(30)
)

min(z[[1]]) + max(z[[2]]) + mean(z[[3]])  # 10.22

?airquality
#===================================================================

mean(subset(airquality, Month == 5)$Wind)
mean(airquality$Wind[airquality$Month == 5])

mean(airquality$Ozone, na.rm = TRUE)


plot(Temp ~ Wind, data = airquality)

set.seed(1337)
x = rnorm(10000)

#===================================================================
# note use abs()
mean(abs(x) > 2)  # 0.0221

set.seed(42)
x = rnorm(100, mean = 0, sd = 10)
mean(f(input = x)) - f()

f = function(input = 42
             ){
  ifelse(input > 0, input, 0)
}
f(c(1,-1))

set.seed(42)
x0 = rep(1, 30)
a = seq(1, 30)
# x1 = (1:30) ^ 2
x1 = a ^ 2
x1 

y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)


mean(y)  # 320.2353

X = cbind(x0, x1)
m
sum(m[17, ] + m[19, ]) # 652

beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
sum(solve(t(X) %*% X) %*% t(X) %*% y)  # 6.427899

sum((X %*% beta_hat - y) ^ 2)  # 42.67698


# week2
pnorm(4, mean = 5, sd = 3, lower.tail = FALSE)
# 0.6305587

#===================================================================
x = 5
(y = -3 + 2.5 * x )

# sd = 2

View(trees)
tree_md = lm(Girth ~ Height, data = trees)
summary(tree_md)$coefficients

# 0.2557471

summary(tree_md)$r.squared
# 0.2696518

x = 0
beta_0 = 10
beta_1 = 5
(y = 10 + 5 * x)
pnorm(6, mean = 10, sd = 4)
# 0.1586553

x = -1
(y = 10 + 5 * x)
pnorm(3, mean = 5, sd = 4, lower.tail = FALSE)
# 0.6914625


x = -2
(y = 10 + 5 * x)
pnorm(3, mean = 0, sd = 4, lower.tail = FALSE)
# 0.2266274


?faithful
faithful_md = lm(eruptions ~ waiting, data = faithful)
summary(faithful_md)$coefficients 
# -1.87401599
# 0.07562795

80 %in% faithful$waiting

faithful_md$fitted.values[faithful$waiting == 80]

# 4.17622

120 %in% faithful$waiting
predict(faithful_md, newdata = data.frame(waiting = 80))
predict(faithful_md, newdata = data.frame(waiting = 120))

# 7.201338


#===================================================================
# RSS
n = nrow(faithful)
(summary(faithful_md)$sigma) ^ 2 * (n - 2)  # 66.56178
sum(faithful_md$residuals ^ 2)


summary(faithful_md)$r.squared
# 0.8114608

sd(faithful_md$residuals)
# 0.495596

# week3
pt(1.3, df = 7, lower.tail = FALSE)
# 0.1173839

#===================================================================
# 2. Consider a random variable Y that has a t distribution with 9 degrees of freedom. 
# Find c such that P[X > c] = 0.025.
qt(0.025, df = 9, lower.tail = FALSE)
# 2.262157

summary(lm(Girth ~ Height, data = trees))$coefficients[2, 4]
# 0.002757815

tree_md = lm(Girth ~ Height, data = trees)
confint(tree_md, level = 0.90)
confint(tree_md, level = 0.90)[2, 2] - confint(tree_md, level = 0.90)[2, 1]
#  0.2656018

#===================================================================
# pay attention to confidence or prediction
ci_beta_1 = confint(tree_model, parm = "Height", level = 0.90)
ci_beta_1[2] - ci_beta_1[1]


predict(tree_md, newdata = data.frame(Height = 79), interval = c("confidence"), level = 0.95)
# 15.12646


#===================================================================
# abs!!! need times 2
pt(2.1, df = 5, lower.tail = FALSE) * 2
# 0.08975325

n = 10
df = n - 2
1 - (1 - 0.9) / 2
crit = qt(0.95, df = 8)
# same as
qt(0.05, df = 8, lower.tail = FALSE)
abs(qt(0.05, df = 8))
# 1.859548

n = 20
sigma = 2
beta_0 = 5
beta_1 = 4
Sxx = 1.5
var_beta_1 = sigma ^ 2 / Sxx
(sd = sqrt(var_beta_1))
pnorm(4.2, mean = 4, sd = sd, lower.tail = FALSE)  # 0.4512616 

summary(faithful_md)$coefficients[2, 2]
# 0.002218541

#===================================================================
# test value is t value
summary(faithful_md)$coefficients[1, 3]
# -11.70212
summary(faithful_md)$coefficients[1, 4]
#  7.359171e-26
summary(faithful_md)$coefficients[2, 3]
# 34.08904

confint(faithful_md, parm = "(Intercept)",level = 0.9)[2]
# -1.609697

(confint(faithful_md, parm = "waiting", level = 0.95)[2] - confint(faithful_md, parm = "waiting", level = 0.95)[1]) / 2
# 0.00436784

predict(faithful_md, newdata = data.frame(waiting = 81), interval = c("confidence"), level = 0.9)[2]
# 4.189899

predict(faithful_md, newdata = data.frame(waiting = 72), interval = c("prediction"), level = 0.99)[3]
# 4.861612

# week4
pf(2.7, df1 = 3, df2 = 5, lower.tail = FALSE)

# 0.1561342

?longley

lmd = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
summary(lmd)
confint(lmd, parm = "GNP", level = 0.9)[1]

summary(lmd)$coefficients[3, 2]


summary(lmd)$fstatistic

p = 9 + 1
n = 30
pf(2.4, df1 = 9, df2 = 20, lower.tail = FALSE)

p = 5+ 1 = 6
n = 20
n - p = 14
pt(-1.3, df = 14) * 2
  
set.seed(42)
X0 = rep(1, 15)
x1 = runif(15)
x2 = runif(15)
x3 = runif(15)
X = cbind(x0, x1, x2, x3)
X
(C = solve(t(X) %*% X))
C[3,3 ]


sqrt(3 ^ 2 * C[3, 3])

set.seed(42)

x1 = runif(15)
x2 = runif(15)
x3 = runif(15)

sigma = 3
x0 = rep(1, 15)
X = cbind(x0, x1, x2, x3)
(C = solve(t(X) %*% X))


providence = data.frame(
  Agriculture = 54,  
  Examination = 23,
  Education = 13,
  Catholic = 60,
  Infant.Mortality = 24
)
swiss_mod = lm(Fertility ~ ., data = swiss)
predict(swiss_mod, newdata = providence)

summary(swiss_mod)$coefficients[3, 4]
summary(swiss_mod)$coefficients["Examination", 4]
confint(swiss_mod, parm = "Catholic", level = 0.99)


newprovidence = data.frame(
  Agriculture = 54,  
  Examination = 23,
  Education = 13,
  Catholic = 60,
  Infant.Mortality = 24
)
predict(swiss_mod, newdata = newprovidence)
predict(swiss_mod, newdata = newprovidence, interval = "confidence",level = 0.95)


summary(swiss_mod)$fstatistic
null_model = lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss)
anova(null_model, swiss_mod)

# week5
library(MASS)
View(Melanoma)

?Melanoma
mean(Melanoma$age[Melanoma$status == 2], na.rm = TRUE)

?airquality
mean(airquality$Wind[airquality$Month == 5], na.rm = TRUE)

x0 = rep(1, 30)
x1 = (1:30) ^ 2

set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)
mean(y)


X = cbind(x0, x1)
X
X[17,]
sum(X[17, ] + X[19,])

y = 10
pnorm(6, mean = 10, sd = 4)

coef(faithful_md)

predict(faithful_md, newdata = data.frame(waiting = 80))

summary(faithful_md)$r.squared
summary(faithful_md)$coefficients[2, 3]

confint(faithful_md, parm = "(Intercept)", level = 0.9)[2]

1 - (1 - 0.9 ) /2 
predict(faithful_md, newdata = data.frame(waiting = 81), interval = "confidence", level = 0.9)


crit = qt(0.95, df = 8)
crit

set.seed(42)
x_values = data.frame(
  x1 = runif(15),
  x2 = runif(15),
  x3 = runif(15)
)

beta_0 = 2
beta_1 = -0.75
beta_2 = 1.5
beta_3 = 0

x0 = rep(1, 15)
X = cbind(x0, x1, x2, x3)
C = solve(t(X) %*% X)
C
