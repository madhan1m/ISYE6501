#Bryson Cook
#ISYE6501, Spring 2018
#Homework 1

#Part 2.2.1a: SVM

install.packages("kknn")
install.packages("kernlab")

library(kknn)
library(kernlab)


mydata = as.matrix(read.csv("credit_card_data.csv",header=FALSE))

model = ksvm(mydata[,1:10], mydata[,11],type="C-svc", kernel="vanilladot",C=100,scaled=TRUE)
# calculate a1-am
a = colSums(model@xmatrix[[1]] * model@coef[[1]])
a

# calculate a0
a0 = -model@b
a0

# see what the model predicts
pred = predict(model,mydata[,1:10])
pred

# see what fraction of the model's predictions match the actual classification
sum(pred == mydata[,11]) / nrow(mydata)


#Part 2.2.1b: Vary C values to view accuracy.  C varies on a logarithmic scale.

Cvals = seq(-10,10)
ans = matrix(, nrow = length(Cvals), ncol = 2)
y = 0
for (x in Cvals){
	model = ksvm(mydata[,1:10], mydata[,11],type="C-svc", kernel="vanilladot",C=1*10^x,scaled=TRUE)
	pred = predict(model,mydata[,1:10])
	accuracy = sum(pred == mydata[,11]) / nrow(mydata)
	y=y+1
	ans[y,1]= 1*10^x
	ans[y,2] = accuracy
	}

#Prints C values then the accuracy:
plot(log10(ans[,1]), ans[,2], main="KSVM Accuracies", 
     xlab="Log(C Value)", ylab="Accuracy")

# As C increases, the max accuracy is found between 1*10^-3 and 100, and also at 100,000.  As C increases, 
# we are decreasing the margin, but error should also get smaller, though only up to 
# a certain point (C = 1*10^5 apparently).


#########################################################################
#Part 3, K-Nearest Neighbor

mydata = data.frame(read.csv("credit_card_data.csv",header=FALSE))
steps = seq(1,100)
answery = matrix(, nrow = length(steps), ncol = 2)
z=1

for (y in steps){
  answerx = matrix(, nrow(mydata), ncol = 1)
  #answerx = matrix(, nrow=100, ncol = 1)
  #for (x in 1:100){
  for (x in 1:nrow(mydata)){
    knn = kknn(V11~., mydata[-x,], mydata[x,], k = y, scale=TRUE) 
    class = round(fitted(knn),0)
    match = class == mydata[x,11]
    answerx[x,1] = match
    total = sum(answerx)
  }
  answery[z,1] = y
  accuracy = total / nrow(mydata)
  answery[z,2] = accuracy
  z = z+1
}
#The max accuracy,0.8532110 occurs at a k-values of 12 and 15
plot(answery[,1], answery[,2], main="K-Nearest Neighbor Accuracies", 
     xlab="k Values", ylab="Accuracy")
