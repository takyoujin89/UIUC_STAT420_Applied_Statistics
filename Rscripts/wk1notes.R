# Week1
# 3.2.1 Vectors
# 4 different ways to create vectors 
# c(), :, seq(), rep()
x = c(1,3,5,7,9)
(y = 1:100)
seq(1.5, 4.2, 0.1)
rep("A", times = 10)
rep(x, times = 3)
# 3.2.1.1 Subsetting
x[-2]  # 1 5 7 9
x[1:3]  # 1 3 5
x[c(1,3,4)]   # 1 5 7 
z = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
x[z]  # 1 3 7 9

# 3.2.2 Vectorization
x = 1:10
x + 1
2 * x
2 ^ x
sqrt(x)
log(x)
x = c(1,3,5,7,8,9)

# 3.2.3 Logical Operators
x > 3  # FALSE FALSE  TRUE  TRUE  TRUE  TRUE
x[x>3]  # 5 7 8 9
# coercion
sum(x > 3)  #  4
as.numberic(x > 3)  # 0 0 1 1 1 1
x[which(x>3)]  # same as x[x>3] 
max(x)  # 9
which(x == max(x))  #6
which.max(x)  #6

# 3.2.4 More Vectorization
x = c(1,3,5,7,8,9)
y = 1:100
x + rep(2, 6)  # 3 5 7 9 10 11 same as x + 2
x > rep(3, 6) # same as x > 3
x + y  #   2   5   8  11  13  15   8  11  14  17  19  21  14...
# 1+1 2+3 3+5 4+7 5+8 6+9 7+1 8+3 ...
( x + y ) - y  # 1 3 5 7 8 9 1 3 5 7 8 9 ...
rep(x, 10) + y  # same as x + y
# all: are all values true?
all(x + y == rep(x, 10) + y)
# identical: test objects for exact equality

# 3.2.5 Matrices
x = 1:9
X = matrix(x, nrow = 3, ncol = 3)
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
# By default the matrix function reorders a vector into columns
# but we can also tell R to use rows instead.
Y = matrix(x, nrow = 3, ncol = 3, byrow = TRUE)
Z = matrix(0, 2, 4)
X[1, 2]
X[1, ]  # 1 4 7
X[, 2]
X[2, c(1, 3)]  # 2 8
x = 1:9
rev(x)  # 9 8 7 6 5 4 3 2 1
rep(1, 9)  # 1 1 1 1 1 1 1 1 1
# rbind: Combine R Objects by Rows or Columns
rbind(x, rev(x), rep(1, 9))
##   [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
## x    1    2    3    4    5    6    7    8    9
##      9    8    7    6    5    4    3    2    1
##      1    1    1    1    1    1    1    1    1
cbind(col_1 = x, col_2 = rev(x), col_3 = rep(1,9))
X = matrix(x, 3, 3)
Y = matrix(y, 3, 3)
X + Y
X - Y
# X * Y is not matrix multilication
X %*% Y
X / Y

# https://www.khanacademy.org/math/linear-algebra/matrix-transformations/matrix-transpose/v/linear-algebra-transpose-of-a-matrix
t(X)
#Z = matrix(c(2, -5, 7, -2, 4, -6), 2, 3, byrow = TRUE))
Z = matrix(c(9, 2, -3, 2, 4, -2, -3, -2, 16), 3, byrow = TRUE)
M = matrix(c(2, -2, -5, 4), 2, 2)
solve(M)  # returns the inverse
##     [,1] [,2]
##[1,]   -2 -2.5
##[2,]   -1 -1.0
# diag: Matrix Diagonals
all.equal(solve(Z) %*% Z, diag(3))
X = matrix(1:6, 2, 3)
##      [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
# dim: Dimensions of an Object, nrows and ncols
dim(X)
rowSums(X)
colSums(X)
rowMeans(X)
colMeans(X)
diag(X)  #4 6
diag(1:5)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    0    0    0
## [2,]    0    2    0    0    0
## [3,]    0    0    3    0    0
## [4,]    0    0    0    4    0
## [5,]    0    0    0    0    5
diag(5)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    0    0    0
## [2,]    0    1    0    0    0
## [3,]    0    0    1    0    0
## [4,]    0    0    0    1    0
## [5,]    0    0    0    0    1

a_vec = c(1, 2, 3)
b_vec = c(2, 2, 2)
c(is.vector(a_vec), is.vector(b_vec))

# %*% dot product, inner product
a_vec %*% b_vec # inner product = 12 = 1*2 + 2*2 + 3*2
a_vec %o% b_vec # outer product 
##      [,1] [,2] [,3]
## [1,]    2    2    2
## [2,]    4    4    4
## [3,]    6    6    6

as.matrix(a_vec) %*% b_vec
##      [,1] [,2] [,3]
## [1,]    2    2    2
## [2,]    4    4    4
## [3,]    6    6    6
 as.matrix(a_vec) %*% as.matrix(b_vec)  # error: non-conformable arguments
 crossprod(a_vec, b_vec)  # inner product
 tcrossprod(a_vec, b_vec)  # outer product 
 
C_mat = matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
D_mat = matrix(c(2, 2, 2, 2, 2, 2), 2, 3)
crossprod(C_mat, D_mat)  # same as t(C_mat) %*% D_mat


# 3.2.6 Lists
list(42, "Hello", TRUE) 
ex_list = list(
	a = c(1, 2, 3, 4),
	b = TRUE,
	c = "Hello",
	d = function(arg = 42){print("Hello World!")},
	e = diag(5)
	)
ex_list$a  # $ returns a named element 	
# ex_list[1] returns a list contain the first element
# ex_list[[1]] returns the first element of the list, in this case, a vector.
ex_list[c("e","a")]
## $e
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    0    0    0
## [2,]    0    1    0    0    0
## [3,]    0    0    1    0    0
## [4,]    0    0    0    1    0
## [5,]    0    0    0    0    1
## 
## $a
## [1] 1 2 3 4

# 3.2.7 Data Frames
# a data frame is a list of same-length vectors
example_data = data.frame(x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
						  y = c(rep("Hello", 9), "Goodbye"),
						  z = rep(c(TRUE, FALSE), 5))

# str: Compactly Display the Structure of an Arbitrary R Object				  
str(example_data)
#'data.frame':	10 obs. of  3 variables:
# $ x: num  1 3 5 7 9 1 3 5 7 9
# $ y: Factor w/ 2 levels "Goodbye","Hello": 2 2 2 2 2 2 2 2 2 1
# $ z: logi  TRUE FALSE TRUE FALSE TRUE FALSE ...

library(readr)

#read_csv("example_data.csv") faster, tibble
#read.csv("example_data.csv") slower, data.frame
ex_from_csv$y  # y is chr
example_data$y  # y is factor

ex_from_csv
example_data

# coerce a data frame to a tibble
library(tibble)
example_data_tb = as_tibble(example_data)
example_data_tb

install.packages("ggplot2")
library(ggplot2)
# head:Return the First or Last Part of an Object
head(mpg, n = 10)  

mpg
str(mpg)  # 234 observations(rows), 11 variables(columns)

?mpg  # pull up the documentation
names(mpg)
mpg$year
mpg$hwy
dim(mpg)
nrow(mpg)
ncol(mpg)

mpg[mpg$hwy > 35, c("manufacturer", "model", "year")]

subset(mpg, subset = hwy > 35, select = c("manufacturer", "model", "year"))

install.packages("dplyr")
library(dplyr)
#  dplyr provides the %>% operator from magrittr. x %>% f(y) turns into f(x, y) 
# you can use it to rewrite multiple operations that you can read left-to-right, top-to-bottom
mpg %>% filter(hwy > 35) %>% select(manufacturer, model, year)


# A data frame operates more like a matrix where it is possible to reduce the subset to a vector.
# A tibble operates more like a list where it always subsets to another tibble.


# 3.3 Programming Basics
# 3.3.1 Control Flow
x = 1
y = 3
if (x > y){
	z = x * y
	print("x is larger than y")
} else{
	z = x + 5 * y
	print("x is less than or equal to y")
}

ifelse(4 > 3, 1, 0)
fib = c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(fib > 6, "Foo", "Bar")

x = 11:15
for (i in 1:5){
	x[i] = x[i] * 2
	}

x = 11:15
x = x * 2  # vectorized operation is better than a loop


# 3.3.2 Functions
standarize = function(x){
  m = mean(x)
  std = sd(x)
  result = (x - m) / std
  result
 }
 
 test_sample = rnorm(n = 10, mean = 2, sd = 5)
test_sample
standarize(x = test_sample)
standarize = function(x) {
  (x - mean(x)) / sd(x)
}
standarize(x = test_sample)

power_of_num = function(num, power = 2){
  num ^ power
}
power_of_num(10)
# with argument names, order doesn't matter
power_of_num(power = 2, num = 10)  # 100
power_of_num(2, 10)  #1024

get_var = function(x, biased = FALSE){
  n = length(x) - 1 * !biased
  (1 / n) * sum((x - mean(x)) ^ 2)
}

get_var(test_sample)
get_var(test_sample, biased = FALSE)
get_var(test_sample, biased = TRUE) 

# 4.1 Summary Statistics
# center
mean(mpg$cty)
sum(mpg$cty) / length(mpg$cty)
median(mpg$cty)  # more robust, less effected by outliers
sort(mpg$cty)[length(mpg$cty)/2]

# spread
var(mpg$cty)
sd(mpg$cty)
mean((mpg$cty - mean(mpg$cty)) ^ 2)
IQR(mpg$cty)

# summary
summary(mpg$cty)

min(mpg$cty)
max(mpg$cty)
range(mpg$cty)

table(mpg$drv)
table(mpg$drv) / nrow(mpg)

# 4.2 Plotting
# 4.2.1 Histogram
hist(mpg$cty)
hist(mpg$cty,
     xlab = "Miles Per Gallon(City)",
     main = "Histogram of MPG(City)",
     breaks = 12,
     col = "dodgerblue",
     border = "darkorange")
#4.2.2 Barplots
barplot(table(mpg$drv))
barplot(table(mpg$drv), 
        xlab = "Drivetrain(f = FWD, r = RWD, 4 = 4WD)",
        ylab = "Frequency",
        main = "Drivetrains",
        col = "dodgerblue",
        border = "darkorange")

#4.2.3 Boxplots
# visualize relationship between a numerical and categorical variable
unique(mpg$drv)
boxplot(mpg$hwy)
# Plot the hwy variable against the drv variable using the dataset mpg
# y ~ x, data argument
boxplot(hwy ~ drv, data = mpg)

# pch: plot character(dot)
boxplot(hwy ~ drv, data = mpg,
xlab   = "Drivetrain (f = FWD, r = RWD, 4 = 4WD)",
ylab   = "Miles Per Gallon (Highway)",
main   = "MPG (Highway) vs Drivetrain",
pch    = 20,
cex    = 2,
col    = "darkorange",
border = "dodgerblue")

# 4.2.4 Scatterplots
plot(hwy ~ displ, data = mpg)
plot(hwy ~ displ, data = mpg, 
     xlab = "Engine Displacement (in Liters)",
     ylab = "Miles Per Gallon (Highway)",
     main = "MPG (Highway) vs Engine Displacement",
     pch  = 20,
     cex  = 2,
     col  = "dodgerblue")

library(lattice)
xyplot(hwy ~ displ, data = mpg)
library(ggplot2)
ggplot2(mpg, aes(x = displ, y = hwy)) + geom_point()   