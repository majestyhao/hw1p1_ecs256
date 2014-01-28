#include <Rcpp.h>
#include <iostream>
using namespace std;
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
SEXP binsearchC(NumericVector x, NumericVector y) {
  int n = x.length();
  int lo = 0, hi = n - 1;
  while (lo + 1 < hi) {
    int mid = (lo + hi)/2;
    if (y(0) == x(mid)) return Rcpp::wrap(mid + 1);
    if (y(0) < x(mid)) {
      hi = mid;
    } else {
      lo = mid;
    }
  }
  if (y(0) <= x(lo)) return Rcpp::wrap(lo + 1);
  if (y(0) < x(hi)) return Rcpp::wrap(hi + 1);
  return Rcpp::wrap(hi + 1 + 1); 
  // return Rcpp::wrap(x(3));
}
