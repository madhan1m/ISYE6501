# Bryson Cook
# HW 5
# ISYE651, Spring 2018

#Part 7.2
rm(list = ls())
cat("\014")
set.seed(1)

#install.packages("stats")
#install.packages("DAAG")
library(stats)
library(DAAG)

input = data.frame(read.table("uscrime.txt", header = TRUE)) #read in data
mydata = input[c(16, 1:15)] #reorder so that crime is the first column (for formula)

# Plot predictors vs. response
predictors = mydata[-1]
headers = list(
  "M",
  "So",
  "Ed",
  "Po1",
  "Po2",
  "LF",
  "M.F",
  "Pop",
  "NW",
  "U1",
  "U2",
  "Wealth",
  "Ineq",
  "Prob",
  "Time"
)
par(mfrow = c(4, 4))
for (i in 1:15) {
  plot(predictors[, i], mydata$Crime, xlab = headers[i])
}


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


f1 = formula(mydata)
model1 = lm(f1, mydata)
summary(model1)
coef1 = model1$coefficients

par(mfrow = c(2, 2))
plot(model1)

crime_prediction = predict.lm(model1, point)
crime_prediction
#that answer of 155 is really low, we are probably overfit, so let's look at the p-values of each point.


Pvalues = summary(model1)$coefficients[, 4]
coef = model1$coefficients


# Eliminate those predictors with a p-value > 0.08.  I know  0.05 is usually the rule,
# but the U2 factor (unemployment rate of urban males 35-39) and Po1
# should be left in as it was considered important by the summary() function.
mydata_fit = mydata[1]
n = 2
for (i in 2:16) {
  if (Pvalues[i] < 0.08) {
    mydata_fit[n] = mydata[i]
    n = n + 1
  }
}

f2 = formula(mydata_fit)
model2 = lm(f2, mydata_fit)
summary(model2)
plot(model2)


crime_prediction_adj = predict.lm(model2, point)

crime_prediction_adj

#Try cross-validation as well:
par(mfrow = c(1, 1))
c1 = cv.lm(mydata, model1, m = 5)
c2 = cv.lm(mydata, model2, m = 5)

# Now compare the models using the R^2 values. From the summaries printed earlier
# we know Model 1's R2 was 0.803 and Model 2's R2 was .766.

SStot = sum((mydata$Crime - mean(mydata$Crime)) ^ 2)

SSc1 = attr(c1, "ms") * nrow(mydata)
SSc2 = attr(c2, "ms") * nrow(mydata)

R2_cvm1 = 1 - SSc1 / SStot
R2_cvm2 = 1 - SSc2 / SStot
R2_cvm1
R2_cvm2

# So we see that the first model was overfit to the data. While the R2 of
# model 1 was initially higher than model 2 using all of the data, by using
# 5 fold cross validation we see that model 2 has a better fit, though it is
# still probably over-fit as we only have a small set of data. As expected, the R2
# of model 2 using cross validation is lower than that of the whole data set.
