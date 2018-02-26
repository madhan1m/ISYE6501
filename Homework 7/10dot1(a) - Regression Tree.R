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

# split = nrow(mydata) / 2
# data_half1 = mydata[1:23,]
# data_half2 = mydata[24:nrow(mydata),]

# tree = tree(Crime ~ .,data = mydata) 
# summary(tree)
# plot(tree); text(tree)
# 
# cv = cv.tree(tree, ,prune.tree, K = 4)
# summary(cv)
# plot(cv); text(cv)

tree = rpart(Crime ~ .,data = mydata)

summary(tree) # detailed summary of splits
print(tree)

plot(tree, uniform=TRUE, main="Crime Rate Data Decision Tree")
text(tree, use.n=TRUE, all=TRUE, cex=.8)
tree$where
names = row.names(tree$frame)
loc = names[tree$where]
groups = cbind(loc, mydata)

group4 = subset.data.frame(groups, loc == 4)
model4 = lm(f1, group4)
summary(model4)
coef4 = model4$coefficients

group5 = subset.data.frame(groups, loc == 5)
model5 = lm(f1, group5)
summary(model5)
coef5 = model5$coefficients

group6 = subset.data.frame(groups, loc == 6)
model6 = lm(f1, group6)
summary(model6)
coef6 = model6$coefficients

group7 = subset.data.frame(groups, loc == 7)
model7 = lm(f1, group7)
summary(model7)
coef7 = model7$coefficients

#The prediction point ends up in node 6, so predicting:
crime_prediction = predict.lm(model6, point)
crime_prediction

#not a very good model.  Try pruning to the lowest cross-validation error from the original tree.
ptree = prune(tree, cp = 0.1481 )

plot(ptree, uniform=TRUE, main="Crime Rate Data Decision Tree (Pruned)")
text(ptree, use.n=TRUE, all=TRUE, cex=.8)
summary(ptree) 
print(ptree)


pnames = row.names(ptree$frame)
ploc = pnames[ptree$where]
pgroups = cbind(ploc, mydata)

pgroup2 = subset.data.frame(pgroups, ploc == 2)
pmodel2 = lm(f1, pgroup2)
summary(pmodel2)
pcoef2 = pmodel2$coefficients

pgroup6 = subset.data.frame(pgroups, ploc == 6)
pmodel6 = lm(f1, pgroup6)
summary(pmodel6)
pcoef6 = pmodel6$coefficients

pgroup7 = subset.data.frame(pgroups, ploc == 7)
pmodel7 = lm(f1, pgroup7)
summary(pmodel7)
pcoef7 = pmodel7$coefficients



#The prediction point again ends up in node 6, so predicting:
pcrime_prediction = predict.lm(pmodel6, point)
pcrime_prediction

# Still not a very good prediction.  I would think we are experiencing a large amount 
# of over-fitting since the node, pgroup6, only contains 10 points of the original data, which is very
# small but still above the 5% minimum (thought 5% of 47 is only 2-3 points of data).
