### Armand Nasseri - 912679383
### Thomas Le - 913081873

# <center> Homework 2

## Problem 1
1.
2.
## Problem 4
1.
2.
3.
 
## Problem 5
#### 1.
The PMF of x:
    P(x = X) = nCx * p^x * (1-p)^n-x
    P = 0.97, n = 10

Code for plot:

 X <- 0:10

 plot(dbinom(X,10,0.97), col="black", main = "Binomial Distribution", xlab = "Number of chips Produced", ylab = "Probability") 






 
#### 2.

The rate of failure p' = 1 - 0.97 = 0.03

> 1-pbinom(1,10,0.03)

> [1] 0.03450656
