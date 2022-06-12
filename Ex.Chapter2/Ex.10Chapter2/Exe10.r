# Ex.10, Chapter 2 ---------------------------------------------------------

# (a) ---------------------------------------------------------------------

library(ISLR2)

df <- Boston
View(df)
?Boston
# The data set contains 506 rows and 13 variables

# (b) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(df$nox, df$crim) # At 0.5-0.6 nox we observe high number of crimes 
plot(df$rm, df$crim) # Houses with 5-7 are correlated with high crime ratios.
plot(df$age, df$crim) # The higher the age variable gets the higher the crime rate.
plot(df$dis, df$crim) # The bigger the distance from Boston employment centers the bigger the crime ratio.

par(mfrow = c(1,1))
plot(df$tax, df$crim)# The higher the tax rate the higher the crimes number, maybe houses with bigger tax get more break in by burglars.

# (c) ---------------------------------------------------------------------

corr.df = cor(df) # Through correlation we can see that indus, nox, nox, lstat and especially age and rm affect the crime ratio.
# By the above we can say see that the number of rooms each house has and how old its is, greatly affect its the crime ratio.
hist(df$crim, col=5, breaks = 25)
#Most suburbs do not have any crime (80% of data falls in crim < 20).

# (d) ---------------------------------------------------------------------
sapply(df, range)
hist(df$tax, col=6, breaks = 25) # High freq at tax>650
nrow(df[df$tax > 650, ]) #How many

hist(df$ptratio, col = 7, breaks = 25) # We observe a high freq at ptratio > 20
nrow(df[df$ptratio > 20, ]) #How many

# (e) ---------------------------------------------------------------------
nrow(df[df$chas==1, ]) # There are 35


# (f) ---------------------------------------------------------------------

median(df$ptratio) # 19.05

# (g) ---------------------------------------------------------------------

.low = which(df$medv<median(df$medv)) # Which rows of df have medv lower than the median of medv

corr.low = cor(df[.low, ]) # Correlate those rows with the rest of the data set

corr.compare = data.frame("low"=corr.low[ ,"medv"],"all"=corr.df[ ,"medv"]) # Create a new data set with 2 columns : low and all

corr.compare$diff = corr.compare$low - corr.compare$all # Add a new collum which will be the difference between the other 2 
View(corr.compare)
hist(corr.compare$diff, xlab = "Diffrences of Correlation", col=2, main = "Correlation") # Plot a histogram which shows which difference is more frequent 

main.corr.compare = corr.compare[order(abs(corr.compare$diff)), ]
View(main.corr.compare)

#The biggest difference by far was observed in the rm variable - average number of rooms per dwelling, so the number of rooms has much less influence in the cheapest 
#houses than the more expensive ones. The least difference was observed in the indus variable, which indicates that there is no difference in 
#the correlation of the studied variable among cheep and expensive houses.

# (h) ---------------------------------------------------------------------
hist(df$rm,col = 7) # A histogramm of the rooms of every appartment

length(which(df$rm>7)) #64

length(which(df$rm>8)) #13

df[which(df$rm>8 & df$medv<30), ]
# Its seems the are some houses with more than 8 rooms and low cost

# END ---------------------------------------------------------------------


