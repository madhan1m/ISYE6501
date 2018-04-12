####### Homework 11 Question 2 using pulp ######

# ---------- Import modules -----------

#import pulp and pandas modules

from pulp import *
import pandas as pd


# ------------ Read data ---------------

data = pd.read_excel("diet.xls") # read all data
data=data[0:64] # rows 0:64 (Excel calls them 1-65) is the food data table
data=data.values.tolist() # Convert dataframe to list


# ------------ Extract individual vectors of data for each nutrient ------------ 
#
# We could do this just by reading vectors, but here it's shown using the python "dict" structure
# which is more efficient

foods = [x[0] for x in data] #list of food names
cost = dict([(x[0], float(x[1])) for x in data]) # cost for each food
calories = dict([(x[0], float(x[3])) for x in data]) # calories for each food
cholesterol = dict([(x[0], float(x[4])) for x in data]) # cholesterol for each food
totalFat = dict([(x[0], float(x[5])) for x in data]) # total fat for each food
sodium = dict([(x[0], float(x[6])) for x in data]) # sodium for each food
carbohydrates = dict([(x[0], float(x[7])) for x in data]) # carbohydrates for each food
dietaryFiber = dict([(x[0], float(x[8])) for x in data]) # fibre for each food
protein = dict([(x[0], float(x[9])) for x in data]) # protein for each food
vitaminA = dict([(x[0], float(x[10])) for x in data]) # vitamin A for each food
vitaminC = dict([(x[0], float(x[11])) for x in data]) # vitamin C for each food
calcium = dict([(x[0], float(x[12])) for x in data]) # calcium for each food
iron = dict([(x[0], float(x[13])) for x in data]) # iron for each food


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

prob += lpSum([calories[f] * foodVars[f] for f in foods]) >= 1500, 'min Calories'
prob += lpSum([calories[f] * foodVars[f] for f in foods]) <= 2500, 'max Calories'

prob += lpSum([cholesterol[f] * foodVars[f] for f in foods]) >= 30, 'min Cholesterol'
prob += lpSum([cholesterol[f] * foodVars[f] for f in foods]) <= 240, 'max Cholesterol'

prob += lpSum([totalFat[f] * foodVars[f] for f in foods]) >= 20, 'min Fat'
prob += lpSum([totalFat[f] * foodVars[f] for f in foods]) <= 70, 'max Fat'

prob += lpSum([sodium[f] * foodVars[f] for f in foods]) >= 800, 'min Sodium'
prob += lpSum([sodium[f] * foodVars[f] for f in foods]) <= 2000, 'max Sodium'

prob += lpSum([carbohydrates[f] * foodVars[f] for f in foods]) >= 130, 'min Carbohydrates'
prob += lpSum([carbohydrates[f] * foodVars[f] for f in foods]) <= 450, 'max Carbohydrates'

prob += lpSum([dietaryFiber[f] * foodVars[f] for f in foods]) >= 125, 'min Fiber'
prob += lpSum([dietaryFiber[f] * foodVars[f] for f in foods]) <= 250, 'max Fiber'

prob += lpSum([protein[f] * foodVars[f] for f in foods]) >= 60, 'min Protein'
prob += lpSum([protein[f] * foodVars[f] for f in foods]) <= 100, 'max Protein'

prob += lpSum([vitaminA[f] * foodVars[f] for f in foods]) >= 1000, 'min Vit_A'
prob += lpSum([vitaminA[f] * foodVars[f] for f in foods]) <= 10000, 'max Vit_A'

prob += lpSum([vitaminC[f] * foodVars[f] for f in foods]) >= 400, 'min Vit_C'
prob += lpSum([vitaminC[f] * foodVars[f] for f in foods]) <= 5000, 'max Vit_C'

prob += lpSum([calcium[f] * foodVars[f] for f in foods]) >= 700, 'min Calcium'
prob += lpSum([calcium[f] * foodVars[f] for f in foods]) <= 1500, 'max Calcium'

prob += lpSum([iron[f] * foodVars[f] for f in foods]) >= 10, 'min Iron'
prob += lpSum([iron[f] * foodVars[f] for f in foods]) <= 40, 'max Iron'


# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output in a readable format ----------- 

print()
print("---------The solution to this diet problem is----------")
for var in prob.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','') )
print()
print("Total cost of food = $%.2f" % value(prob.objective))
        





