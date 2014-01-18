verifyP1b <- function (q) {
  # count the row number of q and put it into n
  n <- nrow(q)
  # transpose q and assign it to newq
  newq <- t(q)
  # replicate 1 for n times and put it in to the nth row of newq
  newq[n, ] <- rep(1, n)
  # replicate 0 for n - 1 times and combine 1 to construct rhs
  rhs <- c(rep(0, n - 1), 1)
  pivec <- solve(newq, rhs)
  return(pivec)
}