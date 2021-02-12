#include <Rcpp.h>
using namespace Rcpp;

//' Clip a numeric vector
//' Clip a numeric vector between lower and upper bounds.
//'
//' @param x a numeric vector.
//' @param a lower bound (double).
//' @param b upper bound (double).
//'
//' @return a numeric vector.
//'
//' @examples
//' v = runif(10)
//' rcpp_clip(v, 3, 7)
// [[Rcpp::export]]
NumericVector rcpp_clip( NumericVector x, double a, double b){
  return clamp( a, x, b ) ;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
rcpp_clip(5, -2, 2)
rcpp_clip(-5, -2, 2)
rcpp_clip(c(-5, 5), -2, 2)
rcpp_clip(c(-5.5, 5.5), -2, 2)
*/
