# "Thomas Le 913081873"
# "Armand Nasser 912679383"

sim1 <- function(nreps) {
  nstops <- 2
  count <- 0
  l1_arr <- 1:nreps
  l2_arr <- 1:nreps
  
  for (i in 1:nreps) {
    passengers <- 0
    for (j in 1:nstops) {
      if (passengers > 0) 
        for (k in 1:passengers)
          if (runif(1) < 0.2)
            passengers <-  passengers - 1
        
      newpass <- sample(0:2, 1, prob=c(0.5, 0.4, 0.1))
      passengers <- passengers + newpass
          
      if (j == 1) {
        l1_arr[i] = passengers
      } else {
        l2_arr[i] = passengers
      }
    }
  }
  
  El1 = mean(l1_arr)
  El2 = mean(l2_arr)
  El2_minus_l1 = mean(l2_arr - l1_arr)
  
  El1sq = mean(l1_arr^2)
  El2sq = mean(l2_arr^2)
  El2_minus_l1sq = mean((l2_arr - l1_arr)^2)
  
  # cat("E(l1) = ", El1, "\n")
  # cat("E(l2) = ", El2, "\n")
  # cat("E(l2-l1) = ", El2_minus_l1, "\n")
  
  # cat("E(l1^2) = ", El1sq, "\n")
  # cat("E(l2^2) = ", El2sq, "\n")
  # cat("E((l2-l1)^2) = ", El2_minus_l1sq, "\n")
  
  varl1 = El1sq - El1^2
  varl2 = El2sq - El2^2
  varl2_minus_l1 = El2_minus_l1sq - El2_minus_l1^2
  
  cat("Var(L1) =", varl1, "\n")
  cat("Var(L2) =", varl2, "\n")
  cat("Var(L2 - L1) =", varl2_minus_l1, "\n")
}

sim2 <- function(r, s, nreps) {
  count <- 0
  total_winnings <- 0
  for (rep in 1:nreps) {
    consech <- 0
    # for m amt of tosses
    for (i in 1:s) {
      toss <- sample(0:1, 1)
      # if heads, increment consecutive heads num
      if (toss) {
        consech <- consech + 1
        # if reached r consecutive heads, break from loop
        if (consech == r) break
      }
      else consech <- 0
    }
    winnings = i
    total_winnings = total_winnings + winnings
    
    # if (consech < k) count <- count + 1
  }
  cat("Expected winnings is:", total_winnings/nreps, "\n");
}

library("partitions")
library("gtools")

permn <- function(x, m, FUN=NULL) {
  p <- permutations(length(x), m, x)
  # print(p)
  
  for (row in 1:nrow(p)) {
    res <- FUN(p[row])
    print(res)
  }
}

# no answer for 3 b

# Problem 5 plot

# X <- 0:10
# plot(dbinom(X,10,0.97), col="black", main = "Binomial Distribution", xlab = "Number of chips Produced", ylab = "Probability") 