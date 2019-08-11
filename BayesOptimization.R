## Implementation of a Bayesian Optimization Algorithm with Gaussian Processes

## load packages
library(GPfit)
library(BBmisc)

## specify black-box function

blackbox_function <-  f <- function(x) {
  return((6 * x - 2)^2 * sin(12 * x - 4))
}


## Define Function for bayes_optimization with Gaussian Processes
bayes_optimGP <- function(initial_startpoints, improvement, blackbox_function, max_iterations){
  
  ## Initialize Points: 
  x <- seq(O, 1, len = 4)
  y <- blackbox_function(x)
  evaluations <- as.data.frame(cbind(x, y))

  for (iteration in 1:max_iterations){
    messagef("Iteration: %i", iteration)
      
    ## step 1: fit GP model to evaluated points
    fit <- GP_fit(
      X = evaluations[, "x"],
      Y = evaluations[, "y"],
      corr = list(type = "exponential", power = 1.95)
      
    ## Step 2: calculate utility to find next point
    x_new <- seq(0, 1, length.out = 100)
    pred <- predict.GP(fit, xnew = data.frame(x = x_new))
    mu <- pred$Y_hat
    sigma <- sqrt(pred$MSE)
    
    probability_improvement <- purrr::map2_dbl(mu, sigma, function(m, s) {
      if (s == 0) return(0)
      else {
        poi <- pnorm((y_best - m) / s)
        # poi <- 1 - poi (if maximizing)
        return(poi)
      }
    })  
  
    expected_improvement <- purrr::map2_dbl(mu, sigma, function(m, s) {
      if (s == 0) return(0)
      else{
        gamma <- (y_best - m) / s
        phi <- pnorm(gamma)
        return(s * (gamma * phi + dnorm(gamma)))
      }
    })
   
    ## define the best x_new which is then used to evaluate   
    x_eval <- x_new[which.max(expected_improvement)]
    y_eval <- f(x_eval)
    evaluations <- rbind(x_eval, y_eval)
    
  pause() 
  } 
  
}






#######################################################################################################
for (iteration in 1:max_iterations) {
  # step 1: fit GP model to evaluated points
  # step 2: calculate utility to find next point
}

## Step 1: Sample n x-values, where x is define between 0 and 1
n <- 4
x <- seq(0, 1, len = 4)

f <- function(x) {
  return((6 * x - 2)^2 * sin(12 * x - 4))
}

y <- f(x)
## Step 2: Store x and f(x) in "evaluations" matrix

evaluations <- as.data.frame(cbind(x, y))

## Step 3: fit GP model to evaluated points
## Function that fits the GP model to the evaluated points



fit <- GP_fit(
    X = evaluations[, "x"],
    Y = evaluations[, "y"],
    corr = list(type = "exponential", power = 1.95)
)
## Step 4: calculate utility to find next point
x_new <- seq(0, 1, length.out = 100)
pred <- predict.GP(fit, xnew = data.frame(x = x_new))
mu <- pred$Y_hat
sigma <- sqrt(pred$MSE)


y_best <- min(evaluations[,  "y"])

probability_improvement <- purrr::map2_dbl(mu, sigma, function(m, s) {
  if (s == 0) return(0)
  else {
    poi <- pnorm((y_best - m) / s)
    # poi <- 1 - poi (if maximizing)
    return(poi)
  }
})

plot(probability_improvement)

expected_improvement <- map(mu, sigma, function(m, s) {
  if (s == 0) return(0)
  gamma <- (y_best - m) / s
  phi <- pnorm(gamma)
  return(s * (gamma * phi + dnorm(gamma)))
})

plot(expected_improvement)
