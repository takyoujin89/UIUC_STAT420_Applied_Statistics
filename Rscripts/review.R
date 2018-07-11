x[-1]
which.max(x)
identical()
all.equal()
any
all

ifelse

# ctrl + shift + A=> reformat code

cbind()

rowSums()
colMeans()

str(example_data)

head(Galton, n = 10)

Galton[Galton$sex == "F", ]$height
# first ten rows with female height > 70
head(subset(Galton, subset = height > 70), n = 10)

table(mpg$class)

diff(pnorm(c(100, 115), mean = 100, sd = 15)

predict(stop_dist_model, newdata = data.frame(speed = 8)

predict(stop_dist_model)

unique(cars$speed)
8 %in% unique(cars$speed)

cars[which(cars$speed == 8)]

confint(stop_dist_model, level=0.99)

# abs!!!
pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE)

confint(tree_model, parm = "Height", level = 0.95)

hidden exptrapolation

length(coef(mpg_model))

# null first then full
anove(null_model, full_model)

1 - pf(0.5533, df1 = 4, df2 = 383)
