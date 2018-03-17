# Bryson Cook
# HW 10
# ISYE651, Spring 2018

#Part 14.1.2
rm(list = ls())
cat("\014")
set.seed(1)

library(stats)

mydata = as.data.frame(
  read.table(
    "breast-cancer-wisconsin.data.txt",
    sep = ",",
    header = FALSE,
    col.names = c(
      "Samplecodenumber",
      "ClumpThickness",
      "UniformityofCellSize",
      "UniformityofCellShape",
      "MarginalAdhesion",
      "SingleEpithelialCellSize",
      "BareNuclei",
      "BlandChromatin",
      "NormalNucleoli",
      "Mitoses",
      "Class"
    )
  )
) #read in data

newdata = data.frame(matrix(, nrow = 699, ncol = 11))
colnames(newdata) =  c(
  "Samplecodenumber",
  "ClumpThickness",
  "UniformityofCellSize",
  "UniformityofCellShape",
  "MarginalAdhesion",
  "SingleEpithelialCellSize",
  "BareNuclei",
  "BlandChromatin",
  "NormalNucleoli",
  "Mitoses",
  "Class"
)
#remove ?'s so the regression model can be trained
i = 1
for (y in seq(1:length(mydata[, 1]))) {
  y = as.integer(y)
  if (mydata[y, 7] != "?") {
    newdata[i, ] = mydata[y, ]
    i = i + 1
  }
}
newdata = newdata[complete.cases(newdata), ]

m1 = lm(BareNuclei ~ . - Samplecodenumber, newdata) #Create regression with new data
summary(m1)
m2 = lm(BareNuclei ~ MarginalAdhesion + NormalNucleoli + Class, newdata) #New model using best factors
summary(m2) #Still not a good p-value, but we'll use it

imputed_data = mydata

i = 1
for (y in seq(1:length(mydata[, 1]))) {
  #Apply regression to model the missing data
  if (imputed_data[y, 7] == "?") {
    x = predict.lm(m2, mydata[y,])
    imputed_data[y, 7] = round(x[1])
    i = i + 1
  }
}

#Double check for missing data in the attribute columns
for (i in seq(2:12)) {
  for (y in seq(1:length(imputed_data[, 1]))) {
    if (imputed_data[y, i] == "?") {
      print(sprintf("Got one at row %s column %s", y, i))
    }
  }
}

#Double check the ? is gone
imputed_data[41,7]
