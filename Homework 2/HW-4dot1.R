#Bryson Cook
#ISYE6501, Spring 2018
#Homework 2

#Part 4.1

#install.packages("stats")
#install.packages("ggplot2")
library(stats)
library(ggplot2)

rm(list = ls())
cat("\014")
set.seed(1)

mydata = data.frame(read.csv("iris.csv", header = TRUE)) #read in data
response = data.frame(mydata[, 6])
training = data.matrix(mydata[, 2:5])


#Need to scale the data
sc_train = training
for (i in 1:4) {
  sc_train[, i] = (training[, i] - min(training[, i])) / (max(training[, i]) -
                                                            min(training[, i]))
}

#Run clustering with all 4 predictors.  "1" cluster isn't really applicable, but it helps with the for loop:
clusters = seq(1, 10)
resp = matrix(, nrow = nrow(sc_train), ncol = length(clusters))
distance = matrix(, nrow = length(clusters), 1)
y = 1
for (x in clusters) {
  km = kmeans(sc_train, x, nstart = 20)
  resp[, x] = km$cluster
  distance[x, ] = km$tot.withinss
  
}


#Comparing the clusters with the known species (though we wouldn't know the actual answer in real life clustering situations)
table(resp[, 2], mydata$Species)
table(resp[, 3], mydata$Species)
table(resp[, 4], mydata$Species)
table(resp[, 5], mydata$Species)
table(resp[, 6], mydata$Species)
table(resp[, 7], mydata$Species)

#Using all four measurements as predictors doesn't really seem to give us much useful information.
#We can keep adding clusters and the clusters will mostly get smaller, but it doesn't actually pertain to much,
#since we have the real data to compare it to.
ggplot(mydata, aes(Petal.Length, Petal.Width, color = resp[, 4])) + geom_point()
ggplot(mydata, aes(Petal.Length, Petal.Width, color = resp[, 4])) + geom_point()

# Plotting out the data vs the known responses shows that the Sepal measurements do not provide a good grouping,
# but the Petal measurements are excellent.  We will redo the previous model creation with only these two predictors.
# We will also only use up to 5 clusters
ggplot(mydata, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
ggplot(mydata, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggsave(
  "Real Response.jpeg",
  width = 6,
  height = 4,
  units = c("in")
)
#Run clustering with all 4 predictors.  "1" cluster isn't really applicable, but it helps with the for loop:
clusters = seq(1, 5)
resp1 = matrix(, nrow = nrow(sc_train), ncol = length(clusters))
distance1 = matrix(, nrow = length(clusters), 1)
y = 1
for (x in clusters) {
  km = kmeans(sc_train[, 3:4], x, nstart = 20)
  resp1[, x] = km$cluster
  distance1[x, ] = km$tot.withinss
}

table(resp1[, 2], mydata$Species)
table(resp1[, 3], mydata$Species)
table(resp1[, 4], mydata$Species)

ggplot(mydata, aes(Petal.Length, Petal.Width, color = resp1[, 2])) + geom_point()
ggsave(
  "2 Clusters.jpeg",
  width = 6,
  height = 4,
  units = c("in")
)
ggplot(mydata, aes(Petal.Length, Petal.Width, color = resp1[, 3])) + geom_point()
ggsave(
  "3 Clusters.jpeg",
  width = 6,
  height = 4,
  units = c("in")
)
ggplot(mydata, aes(Petal.Length, Petal.Width, color = resp1[, 4])) + geom_point()
ggsave(
  "4 Clusters.jpeg",
  width = 6,
  height = 4,
  units = c("in")
)

# As expected, the 3 Cluster solution best matches the actual data, but has 2
# versicolor flowers misclassified and 4 virginica flowers misclassified.
