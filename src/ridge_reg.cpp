// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

#include <omp.h>
#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
using namespace arma;
  
  
//' Fit a single ridge regression model
//'
//' @param X First value
//' @param y Second value
//' @param lambda 
//' @return Vector of penalised regression coefficients
//' @Imports Rcpp, RcppArmadillo
// [[Rcpp::export]]
arma::vec fit_rr(arma::mat X, arma::mat y, double lambda){
  double n = X.n_cols;
  arma::mat XtX = X.t() * X;
  arma::mat I; I.eye(n, n);
  arma::vec beta = inv(XtX + (lambda * I)) * X.t() * y;
  return beta;
}


//' Calculate leave one out cross validation error (OCV) 
//'
//' @param X First value
//' @param y Second value
//' @param lambda 
//' @Imports Rcpp, RcppArmadillo
// [[Rcpp::export]]
double get_ocv(arma::mat X, arma::mat y, double lambda){
  double n = X.n_cols;
  arma::mat XtX = X.t() * X;;
  arma::mat I; I.eye(n, n);
  // use svd decomp to find Aii
  arma::mat U;
  arma::mat V;
  arma::vec s;
  svd_econ(U, s, V, X, "left");
  arma::vec Ad = sum(U * arma::diagmat(s/(pow(s, 2) + lambda)), 1);
  //arma::vec Ad = arma::diagvec(X * inv(XtX + (lambda * I)) * X.t());
  arma::mat mu_hat =  X * inv(XtX + (lambda * I)) * X.t() * y;
  double out = accu(pow(y - mu_hat, 2)/pow(1 - Ad, 2));
  return out;
}

//' Find the optimal regularisation parameter through optimised leave one out cross validation
//'
//' @param v1 First value
//' @param v2 Second value
//' @return Product of v1 and v2
//' @Depends RcppArmadillo
//' @Imports Rcpp, RcppArmadillo
// [[Rcpp::export]]
arma::vec optim_rr(arma::mat X, arma::mat y, arma::vec lams){
  double n = lams.n_elem; 
  arma::vec out(n);
  for (int i = 0; i < n; i++) {
    double lambda = lams[i];
    out[i] = get_ocv(X, y, lambda);
  }
  return out;
}



// defining an Rcpp function that uses openMP to parallelise over an index vector of groups 
// data is split, optim_rr run on each group and a matrix of betas returned (column per group)
// editing to also return a vector of the selected lambdas

//' Fit a ridge regression model to multiple groups in parallel
//'
//' @param X First value
//' @param y Second value
//' @param lams First value
//' @param idx Second value
//' @return List
//' @Imports Rcpp, RcppArmadillo, openmp
// [[Rcpp::export]]
Rcpp::List par_reg(arma::mat X, arma::mat y, arma::vec lams, arma::vec idx)
{
  // Initialise empty betas matrix
  arma::vec groups = unique(idx);
  int ncol = groups.n_elem;
  int nrow = X.n_cols ;
  arma::mat betas(nrow, ncol);
  
  arma::vec opt_lambdas(ncol);
  
  #pragma omp parallel for
  for(int i=0; i<ncol; i++)
  {
  
    // subset the data
    int tod = groups[i];
    arma::uvec rows = find(idx == tod);
    arma::mat X_sub = X.rows(rows);
    arma::mat y_sub = y.rows(rows);

    //use optim_rr to find the optimal lambda
    arma::vec ocvs = optim_rr(X_sub, y_sub, lams);
    uword lam_id = ocvs.index_min(); 
    double opt_lam = lams(lam_id);
    
    //use fit_rr to get the corresponding betas
    arma::vec beta = fit_rr(X_sub, y_sub, opt_lam);
    
    #pragma omp critical
    {
      opt_lambdas[i] = opt_lam;
      betas.col(i) = beta;
    }
  }

  return Rcpp::List::create(Rcpp::Named("lambdas")=opt_lambdas,
                            Rcpp::Named("betas")=betas);
}



