#set working directory
setwd("/cloud/project")

#How to do multiple linear regression in R


MallData <- read.csv("Mall_Customers_extended.csv", header=TRUE)


names(MallData)

y <- MallData$y.LoyaltyScore

x1 <- MallData$Age

x2 <- MallData$Annual.Income..k..

x3 <-MallData$Spending.Score..1.100.

str(MallData$LoyaltyScore)

y <- as.numeric(as.character(MallData$LoyaltyScore))

hist(y)




model2 <- lm(y ~ x1 + x2 + x3, data=MallData)
model2

attach(MallData)


summary(model2)
#Intercepts: There are 3 x-values that are independent: these are x1 is the
#intercept for Age. x2 is the intercept for Annual Income K and x3 is the
#intercept for Spending Score.The estimated Beta for x1 is -0.08975, x2 is
#-0.01474 and x3 is 0.98109. There are 3 different variables with different 
#outcomes.Age, Annual Income K and Spending Score are all continuous variables. 
#For every unit in x1 (Age), there is a decrease of -0.0875 for the outcome 
#(LoyaltyScore). Applies for x2, there is a decrease of -0.01474 for the outcome
#and for x3, there is an increase of 0.98108 for the outcome. 

#Based on the summary, x1 and x2 are not significant. They do not contain the
#asterick. In this multiple Linear Regression ,x3 significantly predicts the 
#outcome. For every unit in x3, the expected outcome increases by 0.98109 
#which is adjusting for x1 and x2.

#Mulitple R-Squared: This is used for Multiple Linear Regression to avoid bias 
#when trying to find associations of x varibles with outcome. When adding more 
#variables R, usually increases, therefore we avoid using Multiple R-squared in 
#this situation. Thus, they both provide the proportion of variability that 
#the x's will explain on the variance of the outcome. It is ideal for it to be 
#high, it should be close to 1. This would mean that all of systemcatic 
#variances are being accounted for, in this model, it is high of 0.894. 

#F-Statistics: The F-statistics is more than 1, it is 560.7, this means that
#p-value is significant as well. When F-statistics is more than 1, it means that
#the systematic variances are being accounted by these x variables and they 
#outweigh the unsystematic variances. 

#p-Value: This would be considered a good model because it is less than 0.05,
#shows that it is statistically significant. 

#Income does not predict more Loyalty stronger than other clusters. The cluster
#that show to have a stronger Loyalty is x3 (Spending Score) with a positive 
#beta of 0.98109. The other betas for x1 and x2 variable show decrease with 
#association of outcome.




#Assessing fit of model
plot(model2)