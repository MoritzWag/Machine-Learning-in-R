## Gradient Boosting and Componentwise Gradient Boosting

## A simple implementation of Gradient Boosting

## required packages
library(rpart)

## Simulation setting:
n = 10
x = seq(0, 10, length.out = n)
X = data.frame(x1 = x)
y = sin(x) + rnorm(n, mean = 0, sd = 0.01)


gradient_boosting <- function(data, predictor, outcome_variable, iterations){
  
  ## first step: Estimate intercept model with L2 loss
  intercept <- mean(outcome_variable)
  
  
  ## calculate pseudo-residuals
  pseudo_residuals <- outcome_variable - intercept
  
  
  
  
}


intercept_test <- mean(y)
pseudo_residuals_test <- y - intercept_test
data_frame_iter1 <- as.data.frame(cbind(pseudo_residuals_test, x))

## fit pseudo-residuals against covariates

est_iter1 <- rpart(pseudo_residuals_test ~ x, data_frame_iter1, maxdepth = 1L, minbucket = 1L)

## predict 

pred_iter1 <- predict(est_iter1, data_frame_iter1)

pseudo_residuals_iter1 <- y - pred_iter1
plot(pseudo_residuals_iter1)
plot(pseudo_residuals_test)

data_frame_iter2 <- as.data.frame(cbind(pseudo_residuals_iter1, x))
est_iter2 <- rpart(pseudo_residuals_iter1 ~ x, data_frame_iter1, maxdepth = 1L, minbucket = 1L)
pred_iter2 <- predict(est_iter2, data_frame_iter2)
pseudo_residuals_iter2 <- y - pred_iter2
plot(pseudo_residuals_iter2)
