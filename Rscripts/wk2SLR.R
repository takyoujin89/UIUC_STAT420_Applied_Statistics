# 5.1
# 5.1
# pdf: probability density functions(density) -- dname
# cdf: cumulative density functions(distribution)  -- pname
# quantile value corresponding to a particular probability  -- qname
# a random draw of values from a particular distribution  -- rname

# Normal Distribution
# what is the probability that a randomly selected person has an IQ below 115?
pnorm(115, mean = 100, sd = 15)   # same as pnorm(1), by default mean = 0, sd = 1
pnorm(1)
# what is the height of the density curve at an IQ of 115?
dnorm(115, mean = 100, sd = 15)
# what is the probability that a randomly selected person has an IQ between 100 and 115
pnorm(115, mean = 100, sd = 15) - pnorm(100, mean = 100, sd = 15)
diff(pnorm(c(100, 115), mean = 100, sd = 15))
# what is the probability that a randomly selected person has an IQ above 130?
pnorm(130, mean = 100, sd = 15, lower.tail = FALSE)  # same as 1 - pnorm(130, mean = 100, sd = 15)
1 - pnorm(130, mean = 100, sd = 15)

# what IQ is needed to be in the top 5% of intelligence?
# qnorm is opposite to pnorm
qnorm(0.05, mean = 100, sd = 15, lower.tail = FALSE)   # same as 1-0.05
qnorm(1 - 0.05, mean = 100, sd = 15)

# what is the probability that someone has an IQ more than two standard deviations from the mean?
pnorm(c(70, 130), mean = 100, sd = 15)
1 - diff(pnorm(c(70, 130), mean = 100, sd = 15))
pnorm(70, mean = 100, sd = 15) * 2  # another way
2 * pnorm(2, lower.tail = FALSE)


dnorm(x = 3, mean = 2, sd = 5)  # the height of the curve at x = 3

pnorm(q = 3, mean = 2, sd = 5)  # Probability that X is less than or equal to 3

qnorm(p = 0.975, mean = 2, sd = 5)  # the quantile for probability 0.975

rnorm(n = 10, mean = 2, sd = 5)  # generate a random sample of size 10

dbinom(x = 10, size = 10, prob = 0.75)  # n and p, size and prob

dname

#7.1 Modeling
View(cars)
str(cars)
dim(cars)
nrow(cars)
ncol(cars)
?cars
# xi: predictor/explanatory, yi = response/target/outcome
# Response = Prediction + Error
# Response = Signal + Noise
# Response = Model + Unexplained
# Response = Deterministic + Random
# Response = Explainable + Unexplainable


# 7. Simple Linear Regression

# 7.1 Modeling

# capital Y indicates a random variable
# and lower case y to denote a potential value of the random variable

# true value of y and fitted value of y
# extrapolation
x = cars$speed
y = cars$dist
y

# 7.2 Least Squares Approach
# vectorized operations
Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x)) ^ 2)
Syy = sum((y - mean(y)) ^ 2)
c(Sxy, Sxx, Syy)  # 5387.40  1370.00 32538.98
beta_1_hat = Sxy / Sxx
beta_0_hat= mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)

#7.2.1 Making Predictions
unique(cars$speed)

8 %in% unique(cars$speed)  # verify if x=8 is an observed value

21 %in% unique(cars$speed)  # FALSE, 21 is not x, interpolation extrapolation

min(cars$speed) < 21 & 21 < max(cars$speed)  # but 21 is in the data range
beta_0_hat + beta_1_hat * 21

range(cars$speed)

range(cars$speed)[1] < 50 & 50 < range(cars$speed)[2] 

beta_0_hat + beta_1_hat * 50

# 7.2.2 residuals
# residual=observed value-predicted value.
#ei = yi ??? y_hati

which(cars$speed == 8)
cars[5, ]

cars[which(cars$speed == 8), ]
16 - (beta_0_hat + beta_1_hat * 8)

y_hat = beta_0_hat + beta_1_hat * x
e = y - y_hat
n = length(e)
s2_e = sum(e ^ 2) / (n - 2)
s2_e

plot(x, y_hat)

s_e = sqrt(s2_e)
# standard deviation of the residuals, also known as residual standard error
# RMSD: root-mean-square error, explains how actual data points agree with a model

s_e  # 15.37959, this tells our estimates of mean stopping distance are "typically" off by 15.38 feet

# 7.3 Decomposition of Variation
SST = sum((y - mean(y)) ^ 2)
SSReg = sum((y_hat - mean(y)) ^ 2)
SSE = sum((y - y_hat) ^ 2)
c(SST = SST, SSReg = SSReg, SSE = SSE)   #32538.98 21185.46 11353.52

s2_e == SSE / (n - 2)  # TRUE

# 7.3.1 Coefficient of Determination
R2 = SSReg / SST
R2


# 7.4 The lm Function
stop_dist_model = lm(dist ~ speed, data = cars)
stop_dist_model
plot(dist ~ speed, data = cars,
     xlab = "Speed in Miles Per Hour",
     ylab = "Stopping Distance in Feet",
     main = "Stopping Distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey"
)
# lwd = line width, lty = line type
abline(stop_dist_model, lwd = 3, col = "darkorange")

stop_dist_model$coefficients
stop_dist_model$fitted.values
stop_dist_model$residuals

cars$dist - stop_dist_model$fitted.values == stop_dist_model$residuals  # not correct
all.equal(cars$dist - stop_dist_model$fitted.values, stop_dist_model$residuals )

coef(stop_dist_model)
fitted(stop_dist_model)
resid(stop_dist_model)

cars$dist == fitted(stop_dist_model) + resid(stop_dist_model)

summary(stop_dist_model)

summary(stop_dist_model)$r.squared

summary(stop_dist_model)$sigma  # sigma is residual standard error


# predict()

predict(stop_dist_model, newdata = data.frame(speed = 8))
predict(stop_dist_model, newdata = data.frame(speed = c(8, 21, 50)))

predict(stop_dist_model, newdata = cars)
# same as predict(stop_dist_model)




# Vectorization
pnorm(0, mean = c(-1, 0, 1), sd = c(2, 1, 0.5))


# More Continuous Distributions
dexp(2, rate = 0.5)
qt(0.975, df = 10)
pf(3.2, df1 = 3, df2 = 10)
rchisq(10, df = 20)

# 7.6 Simulating SLR

# parameters
num_obs = 21
beta_0 = 5
beta_1 = -2 
sigma = 3  # sd 

x_vals = seq(from = 0, to = 10, length.out = num_obs)
x_vals

set.seed(1) 
(epsilon = rnorm(n = num_obs, mean = 0, sd = sigma))

y_vals = beta_0 + beta_1 * x_vals + epsilon
y_vals

# The data, (xi,yi), represent a possible sample from the true distribution
sim_fit = lm (y_vals ~ x_vals)
coef(sim_fit)
plot(y_vals ~ x_vals)
abline(sim_fit)

# data generation function
sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1){
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

set.seed(1) 
sim_data = sim_slr(x = x_vals, beta_0 = 5, beta_1 = -2, sigma = 3)
sim_data$reponse == y_vals

plot(response ~ predictor, data = sim_data,
     xlab = "Speed in Miles Per Hour",
     ylab = "Stopping Distance in Feet",
     main = "Stopping Distance vs Speed",
     pch = 20,
     cex = 2,
     col = "grey",
     ylim = c(-15, 10))
abline(sim_fit, lwd = 3, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")
legend("topright", c("Estimate", "Truth"), lty = c(1, 2), lwd = 2,
       col = c("darkorange", "dodgerblue"))

	   
# linear regression explains
# https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
# http://blog.minitab.com/blog/adventures-in-statistics-2/how-to-interpret-regression-analysis-results-p-values-and-coefficients
# http://www.chegg.com/homework-help/questions-and-answers/scatterplot-shows-relationship-body-weights-measured-kg-heart-weights-measured-g-144-domes-q7795659
