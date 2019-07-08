## Implementation of Support Vector Machines in R


## load packages
library(ggplot2)
library(kernlab)
library(e1071)

## Implementation of SVM with kernlab
set.seed(6)

## Simulate data points
x <- matrix(rnorm(20*2), ncol = 2) #vector with 120 rows and 2 columns
y <- c(rep(-1,10), rep(1,10))

x[y==1,] <- x[y==1, ] + 3/2
dat <- data.frame(x=x, y = as.factor(y))

ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")


# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, dat)


## non-linear specification of   
