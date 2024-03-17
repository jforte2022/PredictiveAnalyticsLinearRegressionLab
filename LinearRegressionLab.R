#' ---
#' title: "Multilinear Regression Lab"
#' author: "Loni Hagen"
#' ---
#'  ###################################
#' ## Chapter 6 - Linear Regression ## 
#' ###################################

### 1 load the required libraries/packages for this chapter (1 point)
#' #Install the package(s) below once on your machine. To do so, uncomment the 
#' #install.packages line(s) below. 
#' install.packages("leaps")

library(leaps) #for subset selection
library(gains) #for gains and lift chart
library(forecast) #for accuracy measures


#' ## Problem 6.1 Predicting Boston Housing Prices. 
#' ##The file BostonHousing.csv contains information collected by the US Bureau of
#' ##the Census concerning housing in the area of Boston, Massachusetts. 
#' ##The dataset includes information on 506 census housing tracts in the Boston 
#' ##area. The goal is to predict the median house price in new tracts based on 
#' ##information such as crime rate, pollution, and number of rooms. The dataset 
#' ##contains 13 predictors, and the response is the median house price (MEDV). 
#' ##Table 6.9 describes each of the predictors and the response.
#' 
#' ##TABLE 6.9 DESCRIPTION OF VARIABLES FOR BOSTON HOUSING EXAMPLE
#' ##CRIM    Per capita crime rate by town
#' ##ZN      Proportion of residential land zoned for lots over 25,000 ft2
#' ##INDUS   Proportion of nonretail business acres per town
#' ##CHAS    Charles River dummy variable (= 1 if tract bounds river; = 0 otherwise)
#' ##NOX     Nitric oxide concentration (parts per 10 million)
#' ##RM      Average number of rooms per dwelling
#' ##AGE     Proportion of owner-occupied units built prior to 1940
#' ##DIS     Weighted distances to five Boston employment centers
#' ##RAD     Index of accessibility to radial highways
#' ##TAX     Full-value property-tax rate per $10,000
#' ##PTRATIO Pupil/teacher ratio by town
#' ##LSTAT   Percentage lower status of the population
#' ##MEDV    Median value of owner-occupied homes in $1000s
#' 
## 2. Q1 through Q9: 1 point each

##Q1: Why should the data be partitioned into training and validation sets? 
#' ##What will the training set be used for? What will the validation set be used
#' ##for?

# The training set will be used to create a prediction model. The validation set will be used to assess the accuracy of the created model.

#' ##Q2: load the data and preprocess

bostonHousing.df <- read.csv("BostonHousing.csv")
  
#' ##Q3: remove the categorical response variable CAT..MEDV

bostonHousing.df <- bostonHousing.df[ , -14]

bostonHousing.df <- bostonHousing.df[ , ]

#' ## Q4: Fit a multiple linear regression model to the median house price 
#' ##(MEDV) as a function of CRIM, CHAS, and RM. Print out the model using summary function. 
#' 
#' #fit the model

selected.df <- bostonHousing.df[, c(1, 4, 6, 13)]
bostonHousingPrice.lm <- lm(MEDV ~ ., data = selected.df)
summary(bostonHousingPrice.lm)
  
#' ## Q5: Split the data to training/validation data set 60/40 percent. Use set.seed(1) to produce the same samples 

set.seed(1)  
trainIndex <- sample(c(1:506), 506 * 0.6)
train.df <- bostonHousing.df[trainIndex, ]
valid.df <- bostonHousing.df[-trainIndex, ]
  
#' ## Q6-8: Use step-wise regression with the two options (exhaustive and backward), 
#' ##to reduce the remaining predictors as follows: Run step-wise on the 
#' ##training set. Choose the top model from each step-wise run. Then use each of
#' ##these models separately to predict the validation set. Compare RMSE. Finally, describe the best model.
#' 
#' 
#' #Q6: step-wise regression with exhaustive search  

exhaustiveSearch <- regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(exhaustiveSearch)
sum
#' 
#' #To find the top three models, the criteria used is as follows:
#' 
##Q6.2 -------------------------------------------------------------------------------------------------------------------------
#Find the 3 highest values of adjusted R-squared by tweaking the following code (replace the variable names with yours).
# top 3 models

models <-  order(sum$adjr2, decreasing = T)[1:3]
models

sum$which

# The best model found from the exhaustive search includes all variables as predictors except INDUS and AGE

bestExhaustiveModel <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT, data = train.df)
summary(bestExhaustiveModel)

#' #Q7: Run step-wise regression using step() function from package stats to perform backward search for variable selection.
#' As was presented in the video lecture, the last model you see is the best model selected using the step() function. 
#' If you see the AIC values, it has the lowest value.  
## create a fitted model using the training data set (include all the available variables)------------------------------------------------

train.df.lm <- lm(MEDV ~ ., data = train.df)
step.backward <- step(train.df.lm, direction = "backward")

#backward regression and print out your model using summary function
  
summary(step.backward)

#the best model according to backward search algorithm includes how many variables?

# The best model includes 10 variables (11 if MEDV is counted as a "variable" even though it isn't a predictor).
 
#' #Q8: using the selected model, conduct predictions on validation set. Then calculate/print the accuracy of the predicted values 
#' compared to the actual value (MEDV)

train.df.lm.predExhaustive <- predict(bestExhaustiveModel, valid.df)
accuracy(train.df.lm.predExhaustive, valid.df$MEDV)

train.df.lm.predBackward <- predict(step.backward, valid.df)
accuracy(train.df.lm.predBackward, valid.df$MEDV)

#' #Q9: based on the results from the exhaustive and backward search (), the best model is with 10 predictors. Write down these predictors.
#' #############################

# The best model is with 10 predictors. The predictors are:  CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT