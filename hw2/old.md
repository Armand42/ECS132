### Armand Nasseri - 912679383
### Thomas Le - 913081873




# <center> Homework 2

## Problem 1
1.
2.
## Problem 4
1. The values the random variable can take are {(1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4), ( 4,1), (4,2), (4,3), (4,4)}
2. The PMF is:

| X| 2 | 3 | 4 | 5 | 6 | 7 | 8 
| :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| P(x) | 1/16  | 2/16  | 3/16 | 4/16  | 3/16 | 2/16 | 1/16

3. The Expected value of X is:

E(x) = x*f(x) = 2*(1/16) + 3*(2/16) + 4*(3/16) + 5*(4/16) + 6(3/16) + 7*(2/16) + 8*(1/16) = 80/16

 
## Problem 5
#### 1.
The PMF of x:
    P(x = X) = nCx * p^x * (1-p)^n-x
    P = 0.97, n = 10

Code for plot:

 X <- 0:10

 plot(dbinom(X,10,0.97), col="black", main = "Binomial Distribution", xlab = "Number of chips Produced", ylab = "Probability") 

Plot goes here:




 
#### 2.

The rate of failure p' = 1 - 0.97 = 0.03

The equation goes here

> 1-pbinom(1,10,0.03)

> [1] 0.03450656
