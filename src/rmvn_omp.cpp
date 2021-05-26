// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(sitmo)]]
// [[Rcpp::depends(BH)]]

#define BOOST_DISABLE_ASSERTS true

#include <boost/random/normal_distribution.hpp>
#include <RcppArmadillo.h>
#include <sitmo.h>

#ifdef _OPENMP
  #include <omp.h>
#else
  #define omp_get_num_threads() 0
  #define omp_get_thread_num() 0
#endif

using namespace Rcpp;
using namespace arma;


//' Sample from a multivariate Gaussian
//'
//' @param n First value
//' @param mu Second value
//' @param sigma
//' @return matrix 
//' @export
// [[Rcpp::export]]
arma::mat rmvn_omp(unsigned int n, arma::vec mu, arma::mat sigma) {
  
  // initialise
  unsigned int d = sigma.n_cols;
  arma::mat cholDec = trimatu(chol(sigma));
  arma::mat A(n, d);
  arma::mat out(n, d);
  
  #pragma omp parallel
  {
    // set seed of each thread to the thread id
    uint32_t coreseed = static_cast<uint32_t>(omp_get_thread_num());
    sitmo::prng_engine engine( coreseed );
    boost::normal_distribution<> normal(0.0, 1.0);
    
    // ?
    #pragma omp for schedule(static)
    for (int irow = 0; irow < n; irow++) {
      for (int icol = 0; icol < d; icol++) {
        A(irow, icol) = normal(engine);
      }
    }
  
    // ?
    #pragma omp for schedule(static)
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < d; j++) {
        for (int k = 0; k < d; k++) {
          out.at(i, j) += mu[j] + A.at(i,k) * cholDec.at(k,j);
        }
        out.at(i, j) += mu[j];
      }
    }
  }
  return out;
}
