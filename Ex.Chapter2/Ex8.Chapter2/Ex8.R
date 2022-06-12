#Exercise 8, Chapter 2

#(a) ---------------------------------------------
College<-read.csv("College.csv")


# (b) ---------------------------------------------------------------------
View(College) 
rownames(College) <- College[, 1] # Each row is given a name corresponding to the appropriate university.
?rownames                                  # R will not try to perform calculations on the row names
College<-College[, -1] # Removing the first column of the data set
View(College) # Checking Results


# (c) ---------------------------------------------------------------------


# c)i ---------------------------------------------------------------------
summary(College)


# c)ii --------------------------------------------------------------------
pairs(College[,2:11]) #(c)ii) We cant use categorical data in the pairs function


# c)iii -------------------------------------------------------------------
Private <- as.factor(College$Private)#
plot(College$Outstate,Private, main = "Outstate vs. Private", xlab = "Outstate", ylab = "Private") 


# c)iv --------------------------------------------------------------------
Elite <- rep("No", nrow(College)) #Creating a variable filled with "No" ,
                                  #that has as many rows as the College data set has
Elite[College$Top10perc > 50] <- "Yes" #Replacing the "No" with "Yes" if the condition is true
Elite <- as.factor(Elite)  # Elite become categorical
College <-data.frame(College,Elite) # Insert Elite into the data set.
summary(College) # There are 78 Elite Colleges
plot(College$Elite,College$Outstate,xlab="Outstate", ylab = "Elite", col="red",main="Outstate vs Elite", border="blue", varwidth=T)


# c)v ---------------------------------------------------------------------
par(mfrow=c(2,2)) 
hist(College$Accept,main =" Historgram of Acceptance",col="red", border = "blue", xlim = c(0,12000), xlab = "Acceptance", breaks = 50)
hist(College$Terminal,main =" Historgram of Terminal",col="green", border = "red", xlim = c(20,100), xlab = "Terminal")
hist(College$Apps,main =" Historgram of Applications",col="purple", border = "blue", xlim = c(0,22000), xlab = "Applications", breaks=50)
hist(College$Enroll,main =" Historgram of Enrollment",col="blue", border = "purple", xlim = c(0,5500),xlab = "Enrollment")


# c)vi --------------------------------------------------------------------

par(mfrow=c(1,1))
plot(College$Top10perc, College$Grad.Rate, main ="Top10percent vs. Grad.Rate", xlab = "Top10percen", ylab ="Grad.Rate")
#Graduation rate is not affected by how good a college is
plot(College$Outstate, College$Grad.Rate, main ="Outstate vs. Grad.Rate", xlab = "Outstate", ylab ="Grad.Rate")
# We can observe that the higher the Outstate variable is the higher the Graduation rate is
plot(College$PhD,College$Grad.Rate)
# The more phds a College has the better it's Graduation Rate is


# THE END -----------------------------------------------------------------

