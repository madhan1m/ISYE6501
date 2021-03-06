#Bryson Cook
#ISYE6501, Spring 2018
#Homework 2

#Part 3.1a

#install.packages("kknn")
library(kknn)

rm(list = ls())
cat("\014")
set.seed(1)

mydata = data.frame(read.csv("credit_card_data.csv", header = FALSE)) #read in data
groups = 10 #10 groups
gs = round(nrow(mydata) / groups, 0) #number of rows per group
steps = seq(1, 20)  #number of neighbors to try
answery = matrix(, length(steps), ncol = groups)


for (y in seq(1, groups)) {
  #outer loop to split data into k subsets for crossvalidation
  start = 1 + (gs * y) - gs
  stop = y * gs
  if (y == groups) {
    stop = nrow(mydata) #So the last group will include any remainder rows
  }
  training = mydata[-(start:stop),] #selects all data except the kth group for training
  validation = mydata[(start:stop),]  #selects the kth group of data for the validation data
  z = 1
  for (i in steps) {
    #second loop to iterate through neighbor values
    answerx = matrix(, nrow(validation), ncol = 1)
    for (x in 1:nrow(validation)) {
      #inner loop to sweep through data set
      knn = kknn(V11 ~ .,
                 training,
                 validation[x,],
                 k = i,
                 scale = TRUE)
      class = round(fitted(knn), 0)
      match = class == validation[x, 11]
      answerx[x, 1] = match
      total = sum(answerx)
    }
    accuracy = total / nrow(validation)
    answery[z, y] = accuracy
    z = z + 1
  }
}

Best_K_Value = which.max(rowMeans(answery))  #This will chose the training set with the higherst accuracy, thus the best K-NN Value
Best_K_Value

answer_final = matrix(, nrow(mydata), ncol = 1)

for (x in 1:nrow(mydata)) {
  #Re-run the chosen model (k-value) on the entire data set to get the accuracy.
  knn = kknn(V11 ~ ., mydata[-x,], mydata[x,],
             k = Best_K_Value,
             scale = TRUE)
  class = round(fitted(knn), 0)
  match = class == mydata[x, 11]
  answer_final[x, 1] = match
}
final_accuracy = sum(answer_final) / nrow(mydata)
final_accuracy
