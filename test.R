binsearch <- function(x,y) {
  n <- length(x)
  lo <- 1
  hi <- n
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (y == x[mid]) return(mid)
    if (y < x[mid]) hi <- mid else lo <- mid
  }
  if (y <= x[lo]) return(lo)
  if (y < x[hi]) return(hi)
  return(hi+1)
}


Rcpp::sourceCpp('test.cpp')
x <- c(5, 10, 15, 21, 25, 7)
y <- 15
# print(binsearch(x, y))
print(system.time(print(binsearch(x, y))))
print(system.time(print(binsearchC(x, y))))

