#Bryson Cook
#ISYE6501, Spring 2018
#Homework 2

#Part 4.1

#install.packages("stats")
library(stats)

rm(list = ls())

mydata = data.frame(read.csv("iris.csv", header = FALSE)) #read in data
mydata = mydata[-1,-1]
rownames(mydata) = 1:nrow(mydata)
response = data.frame(mydata[, 5])
training = data.matrix(mydata[,-5])



clusters = seq(1, 10)
resp = matrix(, nrow = nrow(training), ncol = length(clusters))
distance = matrix(, nrow = length(clusters), 1)
y = 1
for (x in clusters) {
  km = kmeans(training, x, iter.max = 10)
  resp[, x] = km$cluster
  distance[x, ] = km$tot.withinss
}