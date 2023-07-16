# Load the required package
library(readxl)
library(ggplot2)

# Load the data
NBA <- read_excel("NBA.xlsx")

# Check the data
View(NBA)

# Assuming that field goal percentages and wins are numeric data and rest are not
NBA1 <- NBA[,c(2:4)]

# Check the data
View(NBA1)

# Modify column names
colnames(NBA1) <- c("MatchesWon", "FieldGoalPercent", "FieldGoalAllowedPercent")

# Check the data
View(NBA1)

# Check correlations among the variables
cor(NBA1)

# We create a linear regression (lm) model with 'Wins' as the dependent variable and 'FieldGoal%' and 'FieldGoal%Allowed' as independent variables.
models <- lm( MatchesWon ~ FieldGoalPercent + FieldGoalAllowedPercent , data = NBA1)

# Summary of the model
summary(models)

# Compute residuals of the model
res <- residuals(models)

#Print res
print(res)

# Create a new DataFrame
#The predictors for the model
predictors <- c("FieldGoalPercent", "FieldGoalAllowedPercent")
data.frame(FieldGoalPercent = numeric(),FieldGoalAllowedPercent = numeric())
new_prediiction <- data.frame(FieldGoalPercent = 45,FieldGoalAllowedPercent = 44)
           
# We use the predict function to predict the number of Wins
predict(models,new_prediiction)
           
           
# For residual analysis, we can use a plot. This will help us check the homoscedasticity assumption
plot(models, which = 1)
           
# The p-value associated with the overall model can be determined using an ANOVA test
result <- anova(models)
print(result)
p_value <- result$`Pr(>F)`[1]
print(p_value)
