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

