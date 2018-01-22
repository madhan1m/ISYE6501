#Bryson Cook
#ISYE6501, Spring 2018
#Homework 2

#Part 3.1b

#install.packages("kknn")
library(kknn)

rm(list = ls())

mydata = data.frame(read.csv("credit_card_data.csv", header = FALSE)) #read in data

training = data.frame()
validation = data.frame()
test = data.frame()
df = data.frame()
count = 1

for (x in 1:nrow(mydata)) {
  #using rotation to put the 80% of the data into a training set and 10% each into validation and testing sets
  df = mydata[x,]
  if (count >= 11) {
    count = 1
  }
  if (count <= 8) {
    training = rbind(training, df)
  }  else if (count == 9) {
    validation = rbind(validation, df)
  }  else if (count == 10) {
    test = rbind(test, df)
  }
  count = count + 1
}


steps = seq(1, 50)
answery = matrix(, nrow = length(steps), ncol = 2)
z = 1

for (y in steps) {
  #Using the training data, loop through the validation data to find the best k-nn value
  answerx = matrix(, nrow = nrow(validation), ncol = 1)
  for (x in 1:nrow(validation)) {
    knn = kknn(V11 ~ .,
               training,
               validation[x,],
               k = y,
               scale = TRUE)
    class = round(fitted(knn), 0)
    match = class == validation[x, 11]
    answerx[x, 1] = match
    total = sum(answerx)
  }
  answery[z, 1] = y
  accuracy = total / nrow(validation)
  answery[z, 2] = accuracy
  z = z + 1
}

Best_K_Value_validation = which.max(answery[, 2])  #This will chose the index with higherst accuracy, thus the best K-NN Value
Best_K_Value_validation

answerx = matrix(, nrow = nrow(test), ncol = 1)
for (x in 1:nrow(test)) {
  knn = kknn(V11 ~ .,
             training,
             test[x,],
             k = Best_K_Value_validation,
             scale = TRUE)
  class = round(fitted(knn), 0)
  match = class == test[x, 11]
  answerx[x, 1] = match
  test_accuracy = sum(answerx) / nrow(test)
}

test_accuracy
