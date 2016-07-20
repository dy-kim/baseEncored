#include <Rcpp.h>
using namespace Rcpp;

//' @title Vecorized median
//' 
//' @description Calculate median for each chunks of given vector with customizable chunk size.
//'
//' @param x A numeric vector
//' @param chunkSize An integer
//'
//' @export
// [[Rcpp::export]]
NumericVector vectorizedMedian(NumericVector x, int chunkSize) {
  const int n = x.size() / chunkSize;
  std::vector<double> input = Rcpp::as<std::vector<double> > (x);
  NumericVector result(n);
  for (int i = 0; i < n; ++i) {
    std::nth_element(input.begin() + i * chunkSize,
                     input.begin() + i * chunkSize + chunkSize / 2,
                     input.begin() + (i + 1) * chunkSize);
    result[i] = input[i * chunkSize + chunkSize / 2];
  }
  return result;
}
