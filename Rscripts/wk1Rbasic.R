sqrt(2)
pi
exp(1)
pi
exp(log(1))
a = 5

install.packages("mosaicData")
mosaicData::Galton

# ctrl + ENTER run
# ctrl + L clear console

is.numeric(42.5)
is.double(42.5)

is.logical(FALSE)

# colon operator, no space
y = 1:100
y
2
100
# homogenous, coerced to become numbers
c(42, TRUE)
# all become characters
c(42, "Statistics", TRUE)

seq(1, 9, 2)
rep(x, 3)

c(1:10, rep(42, times = 10))[seq(2, 20, by = 2)]
#  2  4  6  8 10 42 42 42 42 42

# logical operators
x == 3 & x != 3
x == 3 | x != 3
which(x > 3)
max(x)
which.max(x)

x = c(1, 3, 5, 7, 8, 9)
log(x)
seq(1.5, 4.2, 0.1)
seq()

silly_fun = function(arg1, arg2, arg3 = 42){
  a = arg1 + arg2 - 3
  b = a * arg3
  c(a, b, a + b, 0)
}
silly_fun(arg1 = 2, arg2 = 3, arg3 = 4)
silly_fun(10, 2, 3)
# code => reformat code or Ctrl + Shift + A

# matrices
Z = matrix(0, 2, 4)
Z

rbind(x, rev(x), rep(1, 9))
cbind(col_1 = x, col_2 = rev(x), col_3 = rep(1,9))

Galton = mosaicData::Galton
Galton
View(Galton)
head(Galton, n = 10)
Galton$sex
# factors: categories, emumerated type
levels(Galton$sex)
Galton[7,3]
Galton[, 2]
names(Galton)

Galton[1, ]  # vector
Galton[3, ]  # vector


Galton[1]  # data frame (898X1)
Galton[1:2]
Galton$father  #  vector
Galton[2]
Galton["father"]  # data frame
Galton[["father"]]  # element, which is a vector

Galton[Galton$sex == "F", ]$height
head(subset(Galton, subset = height > 70), n = 10)

# tibble
install.packages("tibble")

# 1. prints in a same manner
library(tibble)
Galton = as_tibble(Galton)
Galton
# 2. subsetting differently
Galton["height"]  # tibble
Galton$height  # vector
Galton[, 5]  # tibble
Galton[1, 5]  # tibble


# 3. 2. 7 Data Frames
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

