# Bryson Cook
# HW 8
# ISYE651, Spring 2018

#Part 11.1.1 - Stepwise Regression
rm(list = ls())
cat("\014")
set.seed(1)

library(stats)

mydata = data.frame(read.table("uscrime.txt", header = TRUE)) #read in data

null = lm(Crime ~ 1, data = mydata)
full = lm(Crime ~ ., data = mydata)
#just using the stepwise selection, let's see what model we get
stepped = step(null,
               scope = list(lower = null, upper = full),
               direction = "both")
summary(stepped)

# Now we can do it manually, checking the p-values after each step and remove an factors
# that have a p-value >0.05.
sw = step(
  null,
  scope = list(lower = null, upper = full),
  direction = "forward",
  steps = 2
)
summary(sw)
sw1 = step(
  sw,
  scope = list(lower = sw$terms, upper = full),
  direction = "forward",
  steps = 1
)
summary(sw1)
sw2 = step(
  sw1,
  scope = list(lower = sw1$terms, upper = full),
  direction = "forward",
  steps = 1
)
summary(sw2)
sw3 = step(
  sw2,
  scope = list(lower = sw2$terms, upper = full),
  direction = "forward",
  steps = 1
)
summary(sw3)
sw4 = step(
  sw3,
  scope = list(lower = sw3$terms, upper = full),
  direction = "forward",
  steps = 1
)
summary(sw4)
sw5 = step(
  sw4,
  scope = list(lower = sw4$terms, upper = full),
  direction = "forward",
  steps = 1
)
summary(sw5)
# No factors needed to be removed, so it appears using the step() function is the way to go
# in the future.  Double check that the coefficients are the same:
stepped$coefficients
sw5$coefficients

