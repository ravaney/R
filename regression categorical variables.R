# regression using categorical variables
#the categorical variables are re-coded into a set of separate binary variables, this is called DUMMY CODING
# this leads to the creation of a table called CONTRAST MATRIX

install.packages('car')
library(car)

data(Salaries)

model <- lm(salary~ sex,data=Salaries)
summary(model)$coef

contrasts(Salaries$sex)

#if there are more than two levels of categorical variables.

res <- model.matrix(~rank,data=Salaries)
head(res[,-1])
