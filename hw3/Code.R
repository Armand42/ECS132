# "Thomas Le 913081873"                                                                                       
# "Armand Nasser 912679383"

sim1 <- function(nreps, p=0.2, k=3) {
  # get rand distrubtion of nreps size
  N <- rgeom(nreps, p)
  
  # add 1 since we have k = 1, 2 ...
  N <- N + 1
  
  # result is N where N <= k
  res <- N[N <= k]
  return(mean(res))
}

###### PROBLEM C ########

# made it a function as a way to comment out the code
problemC <- function() {
  # get data, skip header row
  original_data <- read.table("./dnc-corecipient/out.dnc-corecipient", skip=1)
  
  # rename cols 1 and 2
  colnames(original_data) <- c("id1", "id2", "nummsgs")
  
  # select rows where id1 < id2 to remove duplicate data, keeping only cols 1 and 2
  dnc <- original_data[original_data$id1 < original_data$id2, c(1,2)]
  
  # create empty vector of 0s; get degrees by counting num occurences of each val
  degrees <- rep(0, nrow(dnc))
  for (i in 1:nrow(dnc)) {
    degrees[dnc[i, "id1"]] <- degrees[dnc[i, "id1"]] + 1
    degrees[dnc[i, "id2"]] <- degrees[dnc[i, "id2"]] + 1
  }
  
  # get max degree, to know what i goes up to
  max_degree <- max(degrees)
    
  # mi is count of recipients having degree i
  mi <- rep(0, max_degree)
  for (i in 1:nrow(filter_data)) {
    # for non zero degrees
    if (degrees[i] != 0) {
      # increment its count
      mi[degrees[i]] <- mi[degrees[i]] + 1
    }
  }
  
  # since calling log on 0 returns -Inf, we replace 0s with NA so plot will ignore the NA vals
  for (i in 1:length(mi)) {
    if (mi[i] == 0)
      mi[i] <- NA
  }
  
  # i goes from 1 to max degree
  i <- c(1:max_degree)
  
  # apply log to i and mi
  log_i <- log(i)
  log_mi <- log(mi)
  
  # apply linear model function and plot
  lm(log_i ~ log_mi)
  plot(log_i, log_mi)
  abline(lm(log_i ~ log_mi))
  
  summary(lm(log_i ~ log_mi))
}


###### END PROBLEM C ##########


# find P(X = i)
daccum <- function(i, k){
  # if out of turns and reach k dots, return success
  if (i == 0 && k <= 0) 
    return (1)
  # else if reach k dots before turns, return failure
  else if (k <= 0) 
    return (0)
  # else if out of turns w/out reaching k dots, return failure
  else if (i == 0) 
    return (0)
  
  return ((1/36)*daccum(i-1, k-2) + 
          (2/36)*daccum(i-1, k-3) + 
          (3/36)*daccum(i-1, k-4) +
          (4/36)*daccum(i-1, k-5) + 
          (5/36)*daccum(i-1, k-6) + 
          (6/36)*daccum(i-1, k-7) +
          (5/36)*daccum(i-1, k-8) + 
          (4/36)*daccum(i-1, k-9) + 
          (3/36)*daccum(i-1, k-10) +
          (2/36)*daccum(i-1, k-11) + 
          (1/36)*daccum(i-1, k-12))
}

# find P(X <= i)
paccum <- function(i, k){
  total <- 0
  
  for(j in 1:i){
    total <- total + daccum(j, k)
  }
  return (total)
}

# find c such that P(X <= c) >= q
qaccum <- function(m, k) {
  support <- ceiling(k/12):ceiling(k/2)
  for (i in support) {
    if (paccum(i, k) >= m) {
      return (i)
    }
  }
  return(0)
}

# generate n independent values of X
raccum <- function(nreps, k) {
  support <- ceiling(k/12):ceiling(k/2)
  samp <- (sample(support, nreps, replace=TRUE))
  return(samp)
  
  # for (i in samp) {
  #   output <- c(output, daccum(i, k))
  # }
  # return(output)
}

