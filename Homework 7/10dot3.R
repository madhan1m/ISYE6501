# Bryson Cook
# HW 7
# ISYE651, Spring 2018

#Part 10.1(b) - Logistic Regression
rm(list = ls())
cat("\014")
set.seed(1)

# install.packages("stats")
# install.packages("boot")
# install.packages("caret")
# install.packages("pROC")
library(stats)
library(boot)
library(caret)
library(pROC)

input = data.frame(read.table("germancredit.txt", header = F)) #read in data
# http://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29

loan = input[21] - 1 #scale the result vector to between 0 and 1
mydata = cbind(loan, input[1:20]) #reorder so that the result is the first column (for formula)
r = nrow(mydata)
set = sample(1:r, size = round(r * .8), replace = FALSE)
train = mydata[set,]
test = mydata[-set,]

#build the model, with categorical factors being treated with the factor() function.
f1 = V21 ~ factor(V1) + V2 + factor(V3) + factor(V4) + V5 + factor(V6) + factor(V7) + V8 + factor(V9) + factor(V10) + V11 + factor(V12) + V13 + factor(V14) + factor(V15) + V16 + factor(V17) + V18 + factor(V19) + factor(V20)
predictors = train[-1]
loan = train[1] - 1

model = glm(f1, family = binomial(link = "logit"), data = train)
summary(model)

# Find and eliminate those predictors with a p-testue < 0.05
data.frame(summary(model)$coef[summary(model)$coef[, 4] <= .05, 4])
f2 = V21 ~ factor(V1) + V2 + factor(V3) + factor(V4) + V5 + factor(V6) + V8 + factor(V9) + factor(V10) + factor(V12) + factor(V14) + factor(V15) + factor(V16) +  factor(V20)
model2 = glm(f2, family = binomial(link = "logit"), data = train)
summary(model2)

#Again, find and eliminate those predictors with a p-testue < 0.05
data.frame(summary(model2)$coef[summary(model2)$coef[, 4] <= .05, 4])
f3 = V21 ~ factor(V1) + V2 + factor(V3) + factor(V4) + V8 + factor(V9) + factor(V10) + factor(V14) + factor(V15) +  factor(V20)
model3 = glm(f3, family = binomial(link = "logit"), data = train)
summary(model3)
model3$coefficients



#Apply cross testidation to double check which model is better:
cv = cv.glm(train, model, K = 10)
cv2 = cv.glm(train, model2, K = 10)
cv3 = cv.glm(train, model3, K = 10)

cv$delta[1]
cv2$delta[1]
cv3$delta[1]



# Now we check what the response threshold should be using the test data:

pred = predict.glm(model3, newdata = test, type = 'response')
actual = test$V21
totalcost = matrix(, 100, ncol = 2)
n = 1
for (thresh in seq(.01, .99, .01)) {
  answerx = matrix(, nrow(test), ncol = 1)
  for (x in 1:length(pred)) {
    if (pred[x] >= thresh) {
      answerx[x] = 1
    } else
      answerx[x] = 0
  }
  cm = confusionMatrix(data = answerx,
                       reference = actual,
                       positive = '0')
  # Now minimize the total cost, which would be cost = FP*5 + FN*1
  cost = cm$table[2, 1] * 5 + cm$table[1, 2] * 1
  totalcost[n, 1] = thresh
  totalcost[n, 2] = cost
  n = n + 1
}
totalcost
totalcost[which.min(totalcost[, 2]), 1]

# From this, the minimum cost is found to be 57 at a minimum threshold of 0.86.
thresh = .86
answerx = matrix(, nrow(test), ncol = 1)
for (x in 1:length(pred)) {
  if (pred[x] >= thresh) {
    answerx[x] = 1
  } else
    answerx[x] = 0
}

cm = confusionMatrix(data = answerx,
                     reference = actual,
                     positive = '0')
cm

#from the confusion matrix we see 142 TN, 57 FN, 0 FP, and 1 TP
roc = roc(test$V21, answerx)
roc
plot(roc)
