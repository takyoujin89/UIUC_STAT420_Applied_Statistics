# 11.1 Dummy Variables

## scipen => no scientific notation, digits controls how many digits are printing
options(scipen = 1, digits = 2)
# use r code instead of hard code

# dummy variables encode categocial information as numeric values
# beta_0, the average fuel efficiency for a automatics transmission when the hp is 0
# beta_1 tells us how the average fuel efficiency changes as the hp increase, it's independent of which transmission type we're using
# beta_2 is an estimate of the difference in average fuel efficiency between manuals and automatics for any hp 
# beta_0 + beta_2, the average fuel efficiency for a manual transmission when the hp is 0

mtcars

plot(mpg ~ hp, data = mtcars, cex = 2)
# using a R trick, automatics are represented by 0(white), am + 1 = 1 => black, manuals are 1, am + 1 = 2=>red 
plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
legend("topright", c("Autamotic", "Manual"), col = c(1, 2), pch = c(1, 2))

mpg_hp_slr = lm(mpg ~ hp, data = mtcars)

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(mpg_hp_slr, lwd = 3, col = "grey")
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

View(mtcars)

mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
coef(mpg_hp_add)[1]
coef(mpg_hp_add)[2]
coef(mpg_hp_add)[3]

int_auto = coef(mpg_hp_add)[1]
int_manu = coef(mpg_hp_add)[1] + coef(mpg_hp_add)[3]
slope_auto = coef(mpg_hp_add)[2]
slope_manu = coef(mpg_hp_add)[2]

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(int_auto, slope_auto, col = 1, lty = 1, lwd = 2)
abline(int_manu, slope_manu, col = 2, lty = 2, lwd = 2)
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

# to test if beta_2 is significant
summary(mpg_hp_add)$coefficients["am", ]

# F test
# P value same and F = t ^ 2
anova(mpg_hp_slr, mpg_hp_add)

# 11.2 Interactions

# read data frame from the web
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# create a dummary variable for foreign vs domestic cars. domestic = 1.
autompg$domestic = as.numeric(autompg$origin == 1)
# remove 3 and 5 cylinder cars (which are very rare.)
autompg = autompg[autompg$cyl != 5,]
autompg = autompg[autompg$cyl != 3,]
# the following line would verify the remaining cylinder possibilities are 4, 6, 8
#unique(autompg$cyl)
# change cyl to a factor variable
autompg$cyl = as.factor(autompg$cyl)



str(autompg)
# 
mpg_disp_add = lm(mpg ~ disp + domestic, data = autompg)

int_for = coef(mpg_disp_add)[1]
int_dom = coef(mpg_disp_add)[1] + coef(mpg_disp_add)[3]

slope_for = coef(mpg_disp_add)[2]
slope_dom = coef(mpg_disp_add)[2]

plot(mpg ~ disp, data = autompg, col = domestic + 1, pch = domestic + 1)
abline(int_for, slope_for, col = 1, lty = 1, lwd = 2)
abline(int_dom, slope_dom, col = 2, lty = 2, lwd = 2)
legend("topright", c("Foreign", "Domestic"), pch = c(1, 2), col = c(1, 2))


# beta_0 is the average mpg for a foreign car with 0 disp.
# beta_1 is the change in average mpg for an increase of one disp, for foreign cars.
# beta_0 + beta 2 is the average mpg for a domestic car with 0 disp.
# beta_1+ beta_3 is the change in average mpg for an increase of one disp, for domestic cars.


# Don't do this
# autompg$x3 = autompg$disp * autompg$domestic 
# do_not_do_this = lm(mpg ~ disp + domestic + x3, data = autompg) 

mpg_disp_int = lm(mpg ~ disp + domestic + disp:domestic, data = autompg)
# interaction bteween these tw as well as any lower order terms (first order term for displancement and domestic)
mpg_disp_int2 = lm(mpg ~ disp * domestic, data = autompg)

coef(mpg_disp_int)
coef(mpg_disp_int2)

summary(mpg_disp_int)
# null model is the one without interaction
# test beta_3
anova(mpg_disp_add, mpg_disp_int)

int_for = coef(mpg_disp_int)[1]
int_for
int_dom = coef(mpg_disp_int)[1] + coef(mpg_disp_int)[3]
int_dom
slope_for = coef(mpg_disp_int)[2]
slope_for
slope_dom = coef(mpg_disp_int)[2] + coef(mpg_disp_int)[4]
slope_dom
plot(mpg ~ disp, data = autompg, col = domestic + 1, pch = domestic + 1)
abline(int_for, slope_for, col = 1, lty = 1, lwd = 2)
abline(int_dom, slope_dom, col = 2, lty = 2, lwd = 2)
legend("topright", c("Foreign", "Domestic"), pch = c(1, 2), col = c(1, 2))

# interactions between two numerical variables
# disp and hp
# How does mpg change based on disp in this model? 
mpg_disp_add_hp = lm(mpg ~ disp + hp, data = autompg)
mpg_disp_int_hp = lm(mpg ~ disp * hp, data = autompg)
summary(mpg_disp_int_hp)

summary(mpg_disp_int_hp)$coefficients["disp:hp", "Pr(>|t|)"]

anova(mpg_disp_add_hp, mpg_disp_int_hp)

coef(mpg_disp_int_hp)
# beta_0 is the estimated average mpg for a car with 0 disp and 0 hp
# beta_1 is is the estimated change in average mpg for an increase in 1 disp, for a car with 0 hp
# beta_2 is the estimated change in average mpg for an increase in 1 hp, for a car with 0 disp.
# beta_3 is an estimate of the modification to the change in average mpg for an increase in disp, for a car of a certain hp (or vice versa).

# 11.3 Factors
is.factor(autompg$domestic)
is.factor(autompg$origin)

autompg$origin[autompg$domestic == 1] = "domestic"
autompg$origin[autompg$domestic == 0] = "foreign"
autompg$origin = ifelse(autompg$domestic == 1, "domestic", "foreign")

is.factor(autompg$origin)
# coerce it into factor
autompg$origin = as.factor(autompg$origin)

# just like enum in Java
levels(autompg$origin)
# integer number staring at 1
as.numeric(autompg$origin)
autompg$origin

str(autompg)
head(autompg$origin, 20)

# mod_dummy: foreign = 0, domestic = 1, reference level
# mod_factor: foreign = 1, domestic = 0 (alphabetic order)
# domestic = 1, foreign = 0
(add_mod_dummy = lm(mpg ~ disp + domestic, data = autompg))
(add_mod_factor = lm(mpg ~ disp + origin, data = autompg))
# same beta_1 coefficients for disp, but different intercept and different sign beta_2
# R create dummy variable using foreign for 1

# dummy Coefficients:
# (Intercept)         disp     domestic  
#   35.48664     -0.05725     -1.30040  
# factor Coefficients:
#   (Intercept)           disp  originforeign  
# 34.18624       -0.05725        1.30040

# additive model
# same result
predict(add_mod_dummy, data.frame(disp = 150, domestic = 1))
predict(add_mod_factor, data.frame(disp = 150, origin = "domestic"))

all.equal(fitted(add_mod_dummy), fitted(add_mod_factor))

# don't do this!
predict(add_mod_dummy, data.frame(disp = 150, domestic = 3.14))

# interaction model
(mod_dummy = lm(mpg ~ disp * domestic, data = autompg))
(mod_factor = lm(mpg ~ disp * origin, data = autompg))


# note  46.0548 = 33.47937 + 12.57547, -0.1569 = -0.05441 + -0.10252  
# and beta_2 and beta_3 have different sign but same magnitude
# Coefficients:
#   (Intercept)           disp       domestic  disp:domestic  
# 46.0548        -0.1569       -12.5755         0.1025  
# 
# Coefficients:
#   (Intercept)                disp       originforeign  disp:originforeign  
# 33.47937            -0.05441            12.57547            -0.10252  


# 11.3.1 Factors with More Than Two Levels
is.factor(autompg$cyl)
levels(autompg$cyl)


mpg_disp_add_cyl = lm(mpg ~ disp + cyl, data = autompg)
summary(mpg_disp_add_cyl)

int_4cyl = coef(mpg_disp_add_cyl)[1]
int_6cyl = coef(mpg_disp_add_cyl)[1] + coef(mpg_disp_add_cyl)[3]
int_8cyl = coef(mpg_disp_add_cyl)[1] + coef(mpg_disp_add_cyl)[4]

slope_all_cyl = coef(mpg_disp_add_cyl)[2]

plot_colors = c("Darkorange", "Darkgrey", "Dodgerblue")
plot(mpg ~ disp, data = autompg, col = plot_colors[cyl], pch=as.numeric(cyl))
abline(int_4cyl, slope_all_cyl, col = plot_colors[1], lty = 1, lwd = 2)
abline(int_6cyl, slope_all_cyl, col = plot_colors[2], lty = 2, lwd = 2)
abline(int_8cyl, slope_all_cyl, col = plot_colors[3], lty = 3, lwd = 2)
legend("topright", c("4 Cylinder", "6 Cylinder", "8 Cylinder"),
       col = plot_colors, lty = c(1, 2, 3), pch = c(1, 2, 3))

# interaction model
(mpg_disp_int_cyl = lm(mpg ~ disp * cyl, data = autompg))

int_4cyl = coef(mpg_disp_int_cyl)[1]
int_6cyl = coef(mpg_disp_int_cyl)[1] + coef(mpg_disp_int_cyl)[3]
int_8cyl = coef(mpg_disp_int_cyl)[1] + coef(mpg_disp_int_cyl)[4]

slope_4cyl = coef(mpg_disp_int_cyl)[2]
slope_6cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[5]
slope_8cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[6]

plot_colors = c("Darkorange", "Darkgrey", "Dodgerblue")
plot(mpg ~ disp, data = autompg, col = plot_colors[cyl], pch = as.numeric(cyl))
abline(int_4cyl, slope_4cyl, col = plot_colors[1], lty = 1, lwd = 2)
abline(int_6cyl, slope_6cyl, col = plot_colors[2], lty = 2, lwd = 2)
abline(int_8cyl, slope_8cyl, col = plot_colors[3], lty = 3, lwd = 2)
legend("topright", c("4 Cylinder", "6 Cylinder", "8 Cylinder"),
       col = plot_colors, lty = c(1, 2, 3), pch = c(1, 2, 3))

anova(mpg_disp_add_cyl, mpg_disp_int_cyl)

# df1 = n- p & df2 = n - q wrong
# df1 = p - q df2 = n - p 
nrow(autompg) - length(coef(mpg_disp_int_cyl))
nrow(autompg) - length(coef(mpg_disp_add_cyl))


# 11.4 Parameterization
new_param_data = data.frame(
  y = autompg$mpg,
  x = autompg$disp,
  v1 = 1 * as.numeric(autompg$cyl == 4),
  v2 = 1 * as.numeric(autompg$cyl == 6),
  v3 = 1 * as.numeric(autompg$cyl == 8))

head(new_param_data)

lm(y ~ x + v1 + v2 + v3, data = new_param_data)

# same slope, three intercepts
lm(y ~ 0 + x + v1 + v2 + v3, data = new_param_data)

# intercept and slope for each line
lm(y ~ 0 + v1 + v2 + v3 + x:v1 + x:v2 + x:v3, data = new_param_data)

# reference intercept and slope
lm(mpg ~ disp * cyl, data = autompg)
# separate intercept and slope
lm(mpg ~ 0 + cyl + disp:cyl, data = autompg)
# separate intercept but reference slop
lm(mpg ~ 0 + disp * cyl, data = autompg)
# separate intercept but reference slop
lm(mpg ~ 0 + disp + cyl + disp : cyl, data = autompg)


all.equal(fitted(lm(mpg ~ disp * cyl, data = autompg)),
          fitted(lm(mpg ~ 0 + cyl + disp : cyl, data = autompg)))

# compare 4 models
# all reference
lm(mpg ~ disp * cyl, data = autompg)
# Coefficients:
#   (Intercept)         disp         cyl6         cyl8    disp:cyl6    disp:cyl8  
# 43.59052     -0.13069    -13.20026    -20.85706      0.08299      0.10817  

# all separate
lm(mpg ~ 0 + cyl + disp : cyl, data = autompg)
# Coefficients:
#   cyl4       cyl6       cyl8  cyl4:disp  cyl6:disp  cyl8:disp  
# 43.59052   30.39026   22.73346   -0.13069   -0.04770   -0.02252  

# separate intercept and reference slope
lm(mpg ~ 0 + cyl * disp, data = autompg)
# Coefficients:
#     cyl4       cyl6       cyl8       disp  cyl6:disp  cyl8:disp  
# 43.59052   30.39026   22.73346   -0.13069    0.08299    0.10817  

lm(mpg ~ 0 + cyl + disp + disp : cyl, data = autompg)
# Coefficients:
#   cyl4       cyl6       cyl8       disp  cyl6:disp  cyl8:disp  
# 43.59052   30.39026   22.73346   -0.13069    0.08299    0.10817  


# 11.5 Larger Models
big_model = lm(mpg ~ disp * hp * domestic, data = autompg)
coef(big_model)

big_model = lm(mpg ~ (disp + hp + domestic) ^ 3, data = autompg)


two_way_int_mod = lm(mpg ~ disp * hp + disp * domestic + hp * domestic, data = autompg)
two_way_int_mod = lm(mpg ~ (disp + hp + domestic) ^ 2, data = autompg)
coef(two_way_int_mod)
anova(two_way_int_mod, big_model)

mean(resid(big_model) ^ 2)
mean(resid(two_way_int_mod) ^ 2)

additive_mod = lm(mpg ~ disp + hp + domestic, data = autompg)
anova(additive_mod, two_way_int_mod)
