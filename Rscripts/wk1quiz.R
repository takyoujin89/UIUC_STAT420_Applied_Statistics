#practice quiz
x = 1:100
#sum(log(x))

set.seed(42)
a_vector = rpois(250, lambda = 6)
# How many of the elements of a_vector are greater than or equal to 5?
# 1st mehtod
length(a_vector[a_vector >= 5])  # 181
# 2nd method
a_vector>=5
sum(a_vector>=5)

x = 1:100
# Create a new vector y, which adds 5 to the elements stored in odd indices of x 
# and subtracts 10 from the elements stored in even indices of x.
remove[c(TRUE, FALSE)]
a = x[c(TRUE, FALSE)] + 5
b = x[c(FALSE, TRUE)] - 10
length(a)
length(b)
y = append(a, b)
length(y)
sd(y)

quiz_list = list(
  x = c(1, 2),
  y = "Hello Quiz Taker",
  z = "z"
)

quiz_list[3]  # a vector with the third element inside
quiz_list[[3]]  # third element
quiz_list["3"]  # NULL
quiz_list$z  # third element

install.packages("MASS")
library(MASS)
hist(Melanoma$age)
View(Melanoma)
# How many individuals in the Melanoma dataset from the MASS package died from a melanoma?
?Melanoma
Melanoma$status
all = Melanoma$status 
died = all[all == 1]
sum(died)
# or just like below
sum(Melanoma$status == 1)  # 57
# What is the average age of individuals in the Melanoma dataset 
# from the MASS package who are alive?
mean(Melanoma$age[Melanoma$status == 2])  # 50.00746

# Which animal in the mammals dataset from the MASS package has the largest brain weight relative to its body weight 
# (that is, the largest brain weight to body weight ratio)?
View(mammals)
ratio = mammals$brain / mammals$body
ratio
which(ratio == max(ratio)) # 11 Ground squirrel
mammals[11, ]     

4 / 0.101

# Create side-by-side boxplots for each of the numeric variables in the iris dataset.
# Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species
?iris
View(iris)
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

sd(iris$Petal.Length)  # 1.765298
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Width)

str(z)
sum(min(z[[1]]) + max(z[[2]]) + mean(z[[3]]))  # 10.22

?airquality
# Using the airquality dataset, what is the average wind speed in May ?
View(airquality)
mean(airquality$Wind[airquality$Month == 5])  # 11.62258

# Using the airquality dataset, what is the average ozone measurement? 
# Hint: read the documentation of any function that returns an unexpected result. 
# You will likely find a solution to the issue.
mean(airquality$Ozone[!is.na(airquality$Ozone)])  # 42.12931

# Using the airquality dataset, create a scatter plot to compare windspeed and temperature. 
# Based on this plot, you believe that
plot(Wind ~ Temp, data = airquality)


set.seed(1337)
x = rnorm(10000)
# What proportion of the elements of x are larger than 2 in magnitude? 
sum(abs(x) > 2)  # 0.0444


#Write a function called f that has a single argument input with a default value of 42 
# which is assumed to be a vector of numeric values. The function should output a vector 
# that is input but with any negative values replaced with 0.
f = function(x = 42){
  ifelse(x > 0, x, 0)
}
f(c(-1,-4,-5,-6))

# -37.70725

# Create three vectors x0, x1, and y. Each should have a length of 30 and store the following:
  
# x0: Each element should be the value 1
# x1: The first 30 square numbers, starting from 1 (so 1, 4, 9, etc.)
# y: The result of running the given code, after creating the other two vectors
# Report the mean of the values stored in y.

x0 = rep(1, 30)
x = 1:30
x ^ 2
x1 = (1:30) ^ 2
x1  
mean(x1)
set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)
mean(y)  # 320.2353
y

# Create a matrix X with columns x0 and x1. Report the sum of the elements in rows 17 and 19.
x0
x1
X = cbind(col_1 = x0, col_2 = x1)
X
X[17, ]
sum(X[17, ])  # 290
sum(X[19, ])  # 362
290 + 362  #652

# beta-hat = (XTX)−1XTy
# Yes. The transpose of matrix X multiplied by matrix X, then take the inverse of that. 
# Multiply what you get by another matrix which is ->(transpose of X multiplied by y)
t(X) %*% X
solve(t(X) %*% X) %*% (t(X) %*% y)
# same as above
solve(t(X) %*% X) %*% t(X) %*% y
sum(solve(t(X) %*% X) %*% (t(X) %*% y))  # 6.427899
y_hat = X %*% (solve(t(X) %*% X) %*% (t(X) %*% y))
y
y_hat
sum (( t(y_hat) - y ) ^ 2)  # 42.67698
