# Bryson Cook
# HW 8
# ISYE651, Spring 2018

#Part 11.1.2 - Lasso
rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("glmnet")

library(glmnet)
library(stats)

mydata = as.data.frame(read.table("uscrime.txt", header = TRUE)) #read in data
sc_data = as.matrix(mydata)
for (i in 1:15) {
  #scale predictors
  sc_data[, i] = (mydata[, i] - min(mydata[, i])) / (max(mydata[, i]) -
                                                       min(mydata[, i]))
}
predictors = sc_data[,1:15]
response = sc_data[, 16]

lasso = cv.glmnet(
  predictors,
  response,
  family = "gaussian",
  alpha = 1,
  nfolds = 5,
  type.measure = "mse"
)
lasso$lambda.min
lasso$lambda.1se
plot(lasso)
coef(lasso, s = lasso$lambda.min)
coef(lasso, s = lasso$lambda.1se)


r = nrow(sc_data)
set = sample(1:r, size = round(r * .8), replace = FALSE)
train = sc_data[set,]
test = sc_data[-set,]

# Using the lambda.min model:
model = lm(Crime~M+So+Ed+Po1+U2+Ineq+Prob+Time, as.data.frame(train))
summary(model)
pred = predict.lm(model, as.data.frame(test))
sse = sum((pred - test[,16]) ^ 2)
sst = sum((test[,16] - mean(test[,16])) ^ 2) #total sum of squares
1 - sse / sst


# Using the .1se model, which is the largest value of lambda
# such that error is within 1 standard error of the minimum:
modelse = lm(Crime~M+So+Ed+Po1+M.F+Ineq+Prob, as.data.frame(train))
summary(modelse)
predse = predict.lm(modelse, as.data.frame(test))
ssese = sum((predse - test[,16]) ^ 2)
sstse = sum((test[,16] - mean(test[,16])) ^ 2) #total sum of squares
1 - ssese / sstse

# The .1se model is slightly better, but both should be considered good quality.