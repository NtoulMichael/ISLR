
# Ex5.Chapter9 Ntoylmperis MIchail ---------------------------------------


# (a) ---------------------------------------------------------------------


library(e1071)
library(MASS)

set.seed(0)

# Part (a):
n <- 5000
p <- 2

x1 <- runif(n) - 0.5
x2 <- runif(n) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)
# y = 1*( x1^2 + x2^2 > 1 ) y = 1*( abs(x1) - abs(x2) > 0 )
df <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

# Part (b):
plot(x1, x2, col = (y + 1), pch = 20, cex = 1.1,)


# (c) ---------------------------------------------------------------------

#Fitting
glm.fit <- glm(y ~ x1 + x2, data = df, family = binomial)


# (d) ---------------------------------------------------------------------
#Prediction

y_hat <- predict(glm.fit, newdata = data.frame(x1 = x1, x2 = x2), type = "response")

predicted_class <- 1 * (y_hat > 0.5)

error <- 1 - sum(predicted_class == y)/length(y)
error


plot(x1, x2, col = (predicted_class + 1))

# (e) ---------------------------------------------------------------------

# After fudging the numbers I concluded in this model.



glm.fit2<- glm(y ~ x1 + x2 + I(x1^2) + I(x2^4) + I(x1 * x2), data = DF, family = "binomial")

y_hat <- predict(glm.fit2, newdata = data.frame(x1 = x1, x2 = x2), type = "response")
predicted_class <- 1 * (y_hat > 0.5)

error <- 1 - sum(predicted_class == y)/length(y)
error

# (f) ---------------------------------------------------------------------

#The plot

plot(x1, x2, col = (predicted_class + 1))

# (g) ---------------------------------------------------------------------
  #Make changes to the data set
  
dfn <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))


# Cross Validation to optimize the model regarding C

tune.out <- tune(svm, y ~ ., data = dfn, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))

plot(tune.out$best.model, dfn)

summary(tune.out$best.model)


# Best c=0.1, 4913 number of vectors
y_hat <- predict(tune.out$best.model, newdata = data.frame(x1 = x1, x2 = x2))
y_hat <- as.numeric(as.character(y_hat))  # turn factor to numerical
error <- 1 - sum(y_hat == y)/length(y)
error

plot(x1, x2, col = (y_hat + 1))

# (h-i) ---------------------------------------------------------------------

tune.out <- tune(svm, y ~ ., data = dfn, kernel = "radial", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
plot(tune.out$best.model, dfn)
summary(tune.out)

y_hat <- predict(tune.out$best.model, newdata = data.frame(x1 = x1, x2 = x2))
y_hat <- as.numeric(as.character(y_hat))  # convert factor responses into numerical values 
error<- 1 - sum(y_hat == y)/length(y)
error


plot(x1, x2, col = (y_hat + 1))




# END ---------------------------------------------------------------------





