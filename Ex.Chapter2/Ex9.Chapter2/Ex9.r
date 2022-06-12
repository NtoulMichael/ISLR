# Ex.9, Chapter 2 ---------------------------------------------------------


# First we load the library ISLR2 and remove missing values from "Auto" --------

library(ISLR2)

df <- Auto

df <- na.omit(Auto)

# (a) ---------------------------------------------------------------------
str(df)

# Using the str() command, R tells us the name variable if the only categorical variable, however this is not he case.

df$cylinders <- as.factor(df$cylinders)

df$year <- as.factor(df$year)

df$origin <- as.factor(df$origin)

# In total 4 of the 9 variables are categorical/qualitative, those variables are cylinders, year, origin, name, the rest are quantitative.

# (b) ---------------------------------------------------------------------

sapply(df[,c(1,3:6)],range) # range for mpg, displacement, horsepower, weight, acceleration .

# (c) ---------------------------------------------------------------------

sapply(df[,c(1,3:6)],mean)# The mean for mpg, displacement, horsepower, weight, acceleration .
sapply(df[,c(1,3:6)],sd)# The sd for mpg, displacement, horsepower, weight, acceleration .
# (d) ---------------------------------------------------------------------
df1 <- df[-c(10:85),] # Removing rows 10-85

sapply(df1[,c(1,3:6)],mean)

sapply(df1[,c(1,3:6)],sd)

# (e) ---------------------------------------------------------------------
pairs(df)
# The higher the Weight,displacement,horsepower seem to get, the less mpg a car has.
#We see an overall increase in mpg over the years. 
#Obviously the name of the car has no correlation with any of the other variables, but it has some with the cylinders and origin variables.
# The more horsepower a car has, the more it weighs.

# (f) ---------------------------------------------------------------------

cor(df[,c(1,3:6)])
#As we can see in the cor output, mpg has a great correlation with displacement horsepower and weight but not so much with acceleration.
# In conclusion the above variables can be used as predictors. With further investigation some of the factor variables can be included.

