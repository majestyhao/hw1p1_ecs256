qBook <- matrix(c(0.25, -1/20, 0, 0, 1/8, -(1/25 + 1/25), 1, 1, 1), nrow = 3, ncol = 3, byrow = TRUE)
q1b <- matrix(c(0, -1/8, (1/25 + 1/25), -(1/8 + 1/8), 1/25, 0, 1, 1, 1), nrow = 3, ncol = 3, byrow = TRUE)
#cat(q)


verifyP1b <- function (q) {
  # count the row number of q and put it into n
  n <- nrow(q)
  # transpose q and assign it to newq
  #newq <- t(q)
  newq <- q
  # replicate 1 for n times and put it in to the nth row of newq
  newq[n, ] <- rep(1, n)
  # replicate 0 for n - 1 times and combine 1 to construct rhs
  rhs <- c(rep(0, n - 1), 1)
  pivec <- solve(newq, rhs)
  cat("pi: ")
  print(pivec)
  cat("w: ")
  print(sum((0:(n[1] - 1)) * pivec))
  return(pivec)
}

verifyP1b(q1b)
