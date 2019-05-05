sim1 <- function(nreps) {
  nstops <- 2
  count <- 0
  l1_arr <- 1:nreps
  l2_arr <- 1:nreps
  
  for (rep in 1:nreps) {
    for (i in 1:nstops) {
      passengers <- 0
      for (j in 1:nstops) {
        for (k in 1:passengers) {
          if (runif(1) < 0.2)
            passengers <-  passengers - 1
          newpass <- sample(0:2, 1, prob=c(0.5, 0.4, 0.1))
          passengers <- passengers + newpass
        }
        if (j == 1) {
          l1_arr[rep] = passengers
        } else {
          l2_arr[rep] = passengers
        }
      }
    }
  }
  
  El1 = mean(l1_arr)
  El2 = mean(l2_arr)
  El2_minus_l1 = mean(l2_arr - l1_arr)
  
  # prob l1  = 1
  sum(l1_arr == 1) / nreps
  
  # Prob l2 - l1 = -1
  sum(l2_arr - l1_arr == -1) / nreps
  
  cat("E(l1) = ", El1, "\n")
  cat("E(l2) = ", El2, "\n")
  cat("E(l2-l1) = ", El2_minus_l1, "\n")
  cat("P(l1 = 1) = ", sum(l1_arr == 1) / nreps, "\n")
  cat("P(l2 - l1 = -1) = ", sum(l2_arr - l1_arr == -1) / nreps, "\n")
}

ngtm <- function(k, m, nreps) {
  count <- 0
  total_winnings <- 0
  for (rep in 1:nreps) {
    consech <- 0
    # for m amt of tosses
    for (i in 1:m) {
      toss <- sample(0:1, 1)
      # if heads, increment consecutive heads num
      if (toss) {
        consech <- consech + 1
        # if reached k consecutive heads, break from loop
        if (consech == k) break
      }
      else consech <- 0
    }
    winnings = i
    total_winnings = total_winnings + winnings
    
    if (consech < k) count <- count + 1
  }
  cat("total winnings is: ", total_winnings, "\n")
  return(total_winnings/nreps)
}

permn <- function(m, n, FUN=NULL) {
  combinations <- combn(m, n, function(x) {
    p <- perms(n)
    res <<- c(1,2)
  })
}