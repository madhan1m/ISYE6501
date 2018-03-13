# Bryson Cook
# HW 8
# ISYE651, Spring 2018

#Part 11.1.3 - Elastic Net
rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("glmnet")

library(glmnet)

mydata = as.data.frame(read.table("uscrime.txt", header = TRUE)) #read in data
sc_data = as.matrix(mydata)
for (i in 1:15) { #scale predictors
  sc_data[, i] = (mydata[, i] - min(mydata[, i])) / (max(mydata[, i]) -
                                                       min(mydata[, i]))
}

predictors = sc_data[,1:15]
response = sc_data[, 16]
resultsmin = matrix(, 19, 2)
colnames(resultsmin) = c('alpha', 'lambda.min')
results1se = matrix(, 19, 2)
colnames(results1se) = c('alpha', 'lambda.1se')

i = 1
for (a in seq(.05,.95,.05)) {
enet = cv.glmnet(
  predictors,
  response,
  family = "gaussian",
  alpha = a,
  nfolds = 5,
  type.measure = "mse"
)

devmin = enet$glmnet.fit$dev.ratio[which(enet$glmnet.fit$lambda == enet$lambda.min)]
dev1se = enet$glmnet.fit$dev.ratio[which(enet$glmnet.fit$lambda == enet$lambda.1se)]

resultsmin[i,1] = a
resultsmin[i,2] = devmin
results1se[i,1] = a
results1se[i,2] = dev1se

i= i+1
}

cbind(resultsmin, results1se)
best_alpha = matrix(, 2, 2)
colnames(best_alpha) = c('alpha', 'lambda')
rownames(best_alpha) = c('.min', '.1se')
best_alpha[1,] = resultsmin[which.max(resultsmin[,2]),]
best_alpha[2,] = results1se[which.max(results1se[,2]),]
best_alpha


r = nrow(sc_data)
set = sample(1:r, size = round(r * .8), replace = FALSE)
train = sc_data[set,]
test = sc_data[-set,]


#Using the lambda.min model
enetmin = cv.glmnet(
  predictors,
  response,
  family = "gaussian",
  alpha = best_alpha[1,1],
  nfolds = 5,
  type.measure = "mse")
coef(enetmin, s = enetmin$lambda.min)


model = lm(Crime~M+So+Ed+Po1+Po2+LF+M.F+NW+U1+U2+Ineq+Prob, as.data.frame(train))
summary(model)
pred = predict.lm(model, as.data.frame(test))
sse = sum((pred - test[,16]) ^ 2)
sst = sum((test[,16] - mean(test[,16])) ^ 2) #total sum of squares
1 - sse / sst

#Using the lambda.1se model
enet1se = cv.glmnet(
  predictors,
  response,
  family = "gaussian",
  alpha = best_alpha[2,1],
  nfolds = 5,
  type.measure = "mse")
coef(enet1se, s = enet1se$lambda.1se)

model1se = lm(Crime~M+Ed+Po1+Po2+M.F+NW+Ineq+Prob, as.data.frame(train))
summary(model1se)
pred1se = predict.lm(model1se, as.data.frame(test))
sse1se = sum((pred1se - test[,16]) ^ 2)
sst1se = sum((test[,16] - mean(test[,16])) ^ 2) #total sum of squares
1 - sse1se / sst1se


           