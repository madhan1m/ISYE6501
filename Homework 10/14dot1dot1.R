# Bryson Cook
# HW 10
# ISYE651, Spring 2018

#Part 14.1.1
rm(list = ls())
cat("\014")
set.seed(1)


mydata = read.table(
  "breast-cancer-wisconsin.data.txt",
  sep = ",",
  header = FALSE
) #read in data

newdata = data.frame(matrix(, nrow = 699, ncol = 11))
i = 1
for (y in seq(1:length(mydata[, 1]))) {
  if (mydata[y, 7] != "?") {
    newdata[i, ] = mydata[y, ]
    i = i + 1
  }
}
newdata = newdata[complete.cases(newdata), ]

for (i in seq(2:11)) {
  #Apply means to missing data in the attribute columns
  for (y in seq(1:length(mydata[, 1]))) {
    if (mydata[y, i] == "?") {
      print(sprintf("Got one at row %s column %s", y, i))
      mydata[y, i] = round(mean(newdata[, i]))
    }
  }
}


for (y in seq(1:length(mydata[, 1]))) {
  #Apply mode to missing data in the Class attribute column
  if (mydata[y, 11] == "?") {
    mydata[y, 11] = round(mode(newdata[, 11]))
  }
}

#Double check the ? is gone
mydata[41,7]
