#include <Rcpp.h>
#include "hiperglm_types.h"
using namespace Rcpp;

// [[Rcpp::export]]

VectorXd QR_solver_eigen(MatrixXd A, VectorXd b){
  if (A.rows() != b.size()) {
    Rcpp::stop("Incompatible matrix and vector dimension.");
  }
  HouseholderQR<MatrixXd> qr(A);
  qr = qr.compute(A);
  return qr.solve(b);
}
