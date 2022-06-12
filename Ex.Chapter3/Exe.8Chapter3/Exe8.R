#Exercise 8, Chapter 3

#(a) ---------------------------------------------
library(ISLR2)
df <- Auto
df <- na.omit(df)
str(df)
View(df)


# (a) ---------------------------------------------------------------------
lm.fit0 <- lm(mpg ~ horsepower, data=df)
summary(lm.fit0)

# i) ----------------------------------------------------------------------

#As we can observe from the output, the p-value for the horsepower variable is very small, so we can determine that there is strong evidence 
#that horsepower is associated with mpg. In conclusion, there is a relationship between the predictor and response.


# ii) ---------------------------------------------------------------------
summary(lm.fit0)$r.sq
# As we can the value of R^2(The percentage of variability in the response that is explained by the predictor) is 0.6059483.
# We can determine by that value, that the relationship between the predictor and the response is strong.


# iii) --------------------------------------------------------------------
coef(lm.fit0)[2]
# As we can see the  coefficient for horsepower is -0.1578447, which is negative.
#By this, we can conclude that if a vehicle has higher horsepower, it will generally have a lower value for mpg.


# iv) ---------------------------------------------------------------------
predict(lm.fit0, data.frame(horsepower = 98), interval = "confidence", level = 0.95)
# As we can see the predicted value of the response is 24.46708 with confidnce intervals as seen in the output.

predict(lm.fit0, data.frame(horsepower = 98), interval = "prediction", level = 0.95)
# On the other hand the output from the code above tells us the prediction intervals.


# (b) ---------------------------------------------------------------------
plot(df$horsepower, df$mpg, main = "Response Vs Predictor", xlab = "Horsepower/Predictor", ylab = "mpg/Response")

abline(lm.fit0, lwd = 3, col=5)


# (c) ---------------------------------------------------------------------

par(mfrow = c(2,2))

plot(lm.fit0)

# By observing the plots from the output we can see that there is a strong pattern in the residuals , indicating non-linearity.
# A solution to this problem can be found by using a quadratic formula or even transformations such Log().
# Finally, there are also some observations with high Levarage and large standarized residuals.

# END ---------------------------------------------------------------------