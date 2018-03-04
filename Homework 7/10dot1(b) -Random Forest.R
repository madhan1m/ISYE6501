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


fit = randomForest(f1, data = mydata, importance = TRUE)
print(fit)
plot()
varImpPlot(fit, main = "Crime Rate Random Forest Variable Importance")
importance(fit)
# Interesting that the Po1, Po2, NW, and Prob variables are considered important in this model,
# as they were not in the previous HW's.

pred = predict(fit)
sse = sum((pred - mydata$Crime) ^ 2)
sst = sum((mydata$Crime - mean(mydata$Crime)) ^ 2)
1 - sse / sst

# This seems much better than the models found in the regular regression tree. Random forests
# have the benefit of reducing overfitting.  Let's try splitting the data into training
# and testing groups

data_train = mydata[1:37, ]
data_test = mydata[38:nrow(mydata), ]

fit2 = randomForest(f1, data = data_train, importance = TRUE)

pred2 = predict(fit2, data_test)
psse = sum((pred2 - data_test$Crime) ^ 2)
psst = sum((data_test$Crime - mean(data_test$Crime)) ^ 2)
1 - psse / psst

# The R2 is much lower than the original, at 0.265.  This decrease is expected though, since
# we are not validating on the same data.
