# Stochastic Hill Climb 
# Following this: https://machinelearningmastery.com/optimize-regression-models/
library(tidyverse)
library(tidymodels)
library(simstudy)
library(modelr)

# Generate some data. 

def <- defData(varname = "age", dist = "normal", formula = 10, 
               variance = 2)
def <- defData(def, varname = "female", dist = "binary", 
               formula = "-2 + age * 0.1", link = "logit", variance = 1.2)
# def <- defData(def, varname = "study_periods", dist = "normal",
#                formula = "1.5 - 0.2 * age + 0.9 * female")

theta1 = c(0.1, 0.8, 0.6, 0.4, 0.6, 0.9, 0.9)
knots <- c(0.25, 0.5, 0.75)

ed_data <- genData(5000, def)

ed_data <- genSpline(dt = ed_data, newvar = "study_periods",
                predictor = "age", theta = theta1,
                knots = knots, degree = 3,
                newrange = "90;160",
                noise.var = 64)

mod_lm <- lm(study_periods ~ age + female, 
               data = ed_data)

summary(mod_lm)
tidy(mod_lm)

ed_data <- ed_data %>% 
  add_predictions(mod_lm)

# Keep going...
