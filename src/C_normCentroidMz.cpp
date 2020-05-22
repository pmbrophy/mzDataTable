#include <Rcpp.h>
using namespace Rcpp;



//' Rcpp function for normCentroidMz
//' SEXP
//' @param mz_vector A sorted numeric vecotor
//' @param mz_upperLimit_vector the mz_vector multiplied by ppmTol*(10^(-6)) and added to mz
//'
//' @export
// [[Rcpp::export]]
List C_normCentroidMz(SEXP mz_vector, SEXP mz_upperLimit_vector) {
  //translate inputs from R to C++
  Rcpp::NumericVector mz(mz_vector);
  Rcpp::NumericVector mz_upperLimit(mz_upperLimit_vector);

  int mz_length = mz.size();

  //generate new NumericVectors
  NumericVector mzGrid (mz_length);
  NumericVector mzGrid_index (mz_length);
  NumericVector mzGrid_upperLimit (mz_length);

  //set value of first element in each vecotr
  mzGrid[0] = mz[0];
  mzGrid_upperLimit[0] = mz_upperLimit[0];

  //loop
  for(int i = 1; i < mz_length; i++) {
    if(mzGrid_upperLimit[i-1] > mz[i]) {
      //Current grid value is same as previous
      mzGrid[i] = mzGrid[i-1];
      //Current index is same as previous
      mzGrid_index[i] = mzGrid_index[i-1];
      //Current upper limit is same as previous
      mzGrid_upperLimit[i] = mzGrid_upperLimit[i-1];
    }
    else {
      //Current grid value new mz
      mzGrid[i] = mz[i];
      //Current index is same as previous
      mzGrid_index[i] = mzGrid_index[i-1] + 1;
      //Current upper limit is updated
      mzGrid_upperLimit[i] = mz_upperLimit[i];
    }
  }

  return List::create(mzGrid, mzGrid_index, mzGrid_upperLimit);
}
