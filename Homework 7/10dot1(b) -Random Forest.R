# Bryson Cook
# HW 7
# ISYE651, Spring 2018

#Part 10.1(a) - Random Forest
rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("randomForest")
library(randomForest)

input = data.frame(read.table("uscrime.txt", header = TRUE)) #read in data
mydata = input[c(16, 1:15)] #reorder so that crime is the first column (for formula)
f1 = formula(mydata)
predictors = mydata[-1]
crime = mydata[1]

point = data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)

fit = randomForest(f1, data = mydata, importance = TRUE)
print(fit)
varImpPlot(fit, main = "Crime Rate Random Forest Variable Importance")
importance(fit)
# Interesting that the Po1, Po2, NW, and Prob variables are considered important in this model,
# as they were not in the previous HW's. 

predict(fit, point)

