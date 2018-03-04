# Bryson Cook
# HW 7
# ISYE651, Spring 2018

#Part 10.1(a) - Regression Tree
rm(list = ls())
cat("\014")
set.seed(1)

# install.packages("tree")
# install.packages("rpart")

library(tree)
library(rpart)


input = data.frame(read.table("uscrime.txt", header = TRUE)) #read in data
mydata = input[c(16, 1:15)] #reorder so that crime is the first column (for formula)
f1 = formula(mydata)
predictors = mydata[-1]
crime = mydata[1]

data_train = mydata[1:35,]
data_test = mydata[36:nrow(mydata),]

tree = rpart(Crime ~ ., data = data_train)

summary(tree) # detailed summary of splits
print(tree)

plot(tree, uniform = TRUE, main = "Crime Rate Data Decision Tree")
text(tree,
     use.n = TRUE,
     all = TRUE,
     cex = .8)
tree$where
names = row.names(tree$frame)
loc = names[tree$where]
groups = cbind(loc, data_train)

pred = predict(tree, data_test)
sse = sum((pred - data_test$Crime) ^ 2)
sst = sum((data_test$Crime - mean(data_test$Crime)) ^ 2) #total sum of squares
1 - sse / sst
# Sum of squares error is greater than the total sum of squares.  This is a bad model.

# Try pruning to the lowest cross-validation error from the original tree.

ptree = prune(tree, cp = 0.1481)

plot(ptree, uniform = TRUE, main = "Crime Rate Data Decision Tree (Pruned)")
text(ptree,
     use.n = TRUE,
     all = TRUE,
     cex = .8)
summary(ptree)
print(ptree)


pnames = row.names(ptree$frame)
ploc = pnames[ptree$where]
pgroups = cbind(ploc, data_train)

ppred = predict(ptree, data_test)
psse = sum((ppred - data_test$Crime) ^ 2) #sum of squared errors
psst = sum((data_test$Crime - mean(data_test$Crime)) ^ 2) #total sum of squares
1 - psse / psst


pgroup2 = subset.data.frame(pgroups, ploc == 2)
pmodel2 = lm(f1, pgroup2)
summary(pmodel2)
#From the summary, reduce the number of factors by pvalue:
pmodel2_a = lm(Crime ~ Wealth + Time + M + So + M.F + Prob, pgroup2)
summary(pmodel2_a) # the model is now worse. I think it is just due to extreme overfitting


pgroup3 = subset.data.frame(pgroups, ploc == 3)
pmodel3 = lm(f1, pgroup3)
summary(pmodel3) #This model is so overfit that 5 coeficients are not defined. Reducing the factors gives:

pmodel3_a = lm(Crime ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW, pgroup2)
summary(pmodel3_a)


# I would think we are experiencing a large amount
# of over-fitting since the node, pgroup3, only contains 11 points of the original data, which is very
# small but still above the 5% minimum (thought 5% of 47 is only 2-3 points of data). The psuedo-R2 calculated
# from the test data still shows that this model is just not good at all.
