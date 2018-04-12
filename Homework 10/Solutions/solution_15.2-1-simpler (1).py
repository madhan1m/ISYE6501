####### Homework 11 Question 2 using PuLP ######

# ---------- Import modules -----------

#import PuLP and pandas modules

from pulp import *
import pandas as pd


# ------------ Read data ---------------

data = pd.read_excel("diet.xls", header = 0) # read all data

dataTable = data[0:64] # rows 0:64 (Excel calls them 1-65) is the food data table
dataTable = dataTable.values.tolist() # Convert dataframe to list

nutrientNames = list(data.columns.values) # column headers (nutrient names are in columns 3-13; Excel calls them D-N)

minVal = data[65:66].values.tolist() # minimum nutrient values
maxVal = data[66:67].values.tolist() # maximum nutrient values
    

# ------------ Extract individual vectors of data ------------ 
#
# We could do this just by reading vectors, but here it's shown using the python "dict" structure
# which is more efficient

foods = [j[0] for j in dataTable] #list of food names

cost = dict([(j[0], float(j[1])) for j in dataTable]) # cost for each food

nutrients = []
for i in range(0,11): # for loop running through each nutrient: 11 times starting with 0
    nutrients.append(dict([(j[0], float(j[i+3])) for j in dataTable])) # amount of nutrient i in food j


# ------------ Create a new LP Problem ------------ 
#
# This problem is a minimization problem (find the *lowest* cost), so "LpMinimize" is the second parameter.

prob = LpProblem('Food optimization', LpMinimize) # 2 parameters: "name" and "sense"


# ------------ Define the variables ---------------
#
# One variable (we chose the name "foodVars") for each food.
# Lower limit of each variable is 0, since we can't eat negative amounts of anything.

foodVars = LpVariable.dicts("Foods", foods, 0)


# ------------ Create objective function ------------ 
#
# Note that the first function we add is taken to be the objective function

prob += lpSum([cost[f] * foodVars[f] for f in foods]), 'Total Cost'


# ------------ Add constraints for each nutrient ------------ 

for i in range(0,11): # for loop running through each nutrient: 11 times starting with 0
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) >= minVal[0][i+3], 'min nutrient ' + nutrientNames[i]
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) <= maxVal[0][i+3], 'max nutrient ' + nutrientNames[i]


# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output in a readable format ----------- 

print()
print("---------The solution to the diet problem is----------")
for var in prob.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','') )
print()
print("Total cost of food = $%.2f" % value(prob.objective))        
