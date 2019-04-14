# Problem 1
# i being starting square w/ values 0-7
# k being num of turns to reach or pass square 0

# count only the ones that land on or pass 0 on the kth turn;
# if we have already passed 0 on a turn prior to the kth turn, do not count that
# see pik(5,2) = 0.30556 as example

pik <- function(i, k) {
  # if land/pass 0th square and not on the kth turn, do not count this series of rolls
  if (i > 7 && k != 0) {
    return(0)
  }
  # else if land/pass 0th square, and on kth turn, then success
  else if (i > 7 & k == 0) {
    return(1)
  }
  # else if out of turns and havent passed square 7 at all, return 0
  else if (k == 0) {
    return(0)
  }
  # recursive function, counting each possible roll of the die, subtracting 1 from each turn
  else {
    return(
      ((1/6) * pik(i+1, k-1)) +
      ((1/6) * pik(i+2, k-1)) +
      ((1/6) * pik(i+3, k-1)) +
      ((1/6) * pik(i+4, k-1)) +
      ((1/6) * pik(i+5, k-1)) +
      ((1/6) * pik(i+6, k-1)) 
    )
  }
}

# Problem 2

# Suppose it is known that X1 = X2, find hte probabilties that there were 0, 1, 
# or 2 collisions during those two epochs.
sim <- function(p,q,nreps) {
  
  # we want P(c = 0 | x1 = x2)
  # P(c=1 | x1 = x2)
  # P(c=2 | x1 = x2)
  
  countx1eqx2 <- 0
  countc0x1eqx2 <- 0
  countc1x1eqx2 <- 0
  countc2x1eqx2 <- 0
  
  # assume n = 2 nodes
  # assume X0 = 2 ie both nodes start out active
  for (i in 1:nreps) {
    num_collisions <- 0  # no collisions so far
    numsend <-0  # no messages sent so far
    
    "simulate A and B's decision on whether to send in epoch 1"
    for (j in 1:2) {
      # check both nodes, see if they send
      if (runif(1) < p) numsend <- numsend + 1
    }
    
    # if neither sent, X1 still has 2 active nodes
    if (numsend == 0) X1 <- 2
    # else only 1 sent, so only 1 active node
    else if (numsend == 1) X1 <- 1
    
    # if both sent, there was a collision, so still 2 active nodes
    else if (numsend == 2) {
      num_collisions <- num_collisions + 1
      X1 <- 2
    }
    
    "simulate for epoch 2"
    # save num active from X1
    numactive <- X1
    
    # possible states for end of X1: X1 == 1 or X1 == 2
    
    # if X1 == 1, then there is 1 node with the chance to activate at the beginning of X2
    # runif(1) < q is the chance that the inactive node activates, thus increment numactive
    # at the start of epoch 2
    if (X1 == 1 && runif(1) < q) numactive <- numactive + 1
    
    # now check possible sendings
    if (numactive == 1) {
      # if sends, then no more active nodes at end of epoch 2 aka, X2 <- 0
      if (runif(1) < p) {
        X2 <- 0
      }
      # else, does not send, X2 has 1 active node
      else {
        X2 <- 1
      }
    }
    else {  # else numactive == 2
      numsend <- 0  # reset numsend
      
      for (i in 1:2) {
        # check both nodes, see if they send
        if (runif(1) < p) numsend <- numsend + 1
      }
      
      # if neither sent, X2 still has 2 active nodes
      if (numsend == 0) X2 <- 2
      # else only 1 sent, so only 1 active node
      else if (numsend == 1) X2 <- 1
      
      # if both sent, there was a collision, so still 2 active nodes
      else if (numsend == 2) {
        num_collisions <- num_collisions + 1
        X2 <- 2
      }
    }
    
    " Finished simulating epoch 1 and epoch 2"
    # now count collisions if X1 == X2
    if (X1 == X2) {
      countx1eqx2 <- countx1eqx2 + 1
      if (num_collisions == 0) countc0x1eqx2 <- countc0x1eqx2 + 1
      else if (num_collisions == 1) countc1x1eqx2 <- countc1x1eqx2 + 1
      else if (num_collisions == 2) countc2x1eqx2 <- countc2x1eqx2 + 1
    }
  }
  
  # cat("Number of occurences of X1 = X2:", countx1eqx2, "\n")
  cat("P(c = 0 | X1 = X2) = ", countc0x1eqx2/nreps, "\n")
  cat("P(c = 1 | X1 = X2) = ", countc1x1eqx2/nreps, "\n")
  cat("P(c = 2 | X1 = X2) = ", countc2x1eqx2/nreps, "\n")
}