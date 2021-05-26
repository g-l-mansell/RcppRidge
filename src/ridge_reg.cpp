// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <omp.h>
#include <RcppArmadillo.h>
  
  
//' Fit a single ridge regression model
//'
//' @param X Data matrix
//' @param y Column matrix of responses
//' @param lambda Numeric hyperparameter controlling the strength of the L2 penalisation (non-negative)
//' @return Vector of penalised regression coefficients
//' @export
// [[Rcpp::export]]
arma::vec fit_rr(arma::mat X, arma::mat y, double lambda){
  double n = X.n_cols;
  arma::mat XtX = X.t() * X;
  arma::mat I; I.eye(n, n);
  arma::vec beta = inv(XtX + (lambda * I)) * X.t() * y;
  return beta;
}


//' Predict new sample responses using a tuned regression model
//'
//' @param X Data matrix of test samples
//' @param beta Vector of regression coefficients
//' @return Vector of fitted values
//' @export
// [[Rcpp::export]]
arma::vec predict_rr(arma::mat X, arma::vec beta){
  return X * beta;
}

//' Calculate leave one out cross validation error (OCV) for a single regression model
//'
//' @param X Data matrix
//' @param y Column matrix of responses
//' @param lambda Numeric hyperparameter controlling the strength of the L2 penalisation (non-negative)
//' @return Numeric OCV
//' @export
// [[Rcpp::export]]
double get_ocv_once(arma::mat X, arma::mat y, double lambda){
  double p = X.n_cols;
  double n = X.n_rows;
  arma::mat I; I.eye(p, p);
  arma::mat B = (X.t() * X) + (I * lambda);
  
  //computing A directly
  //arma::mat A = X * solve(B, X.t());
  //arma::vec Ad = arma::diagvec(A);
  //arma::vec mu_hat =  A * y;
  
  //or avoiding storing A
  arma::vec Ad = arma::diagvec(X * solve(B, X.t()));
  arma::vec mu_hat = X * solve(B, X.t()) * y;
  
  double out = sum(pow(y - mu_hat, 2)/pow(1 - Ad, 2))/n;
  return out;
}

//' Fast calculate leave one out cross validation error (OCV)
//'
//' Fast calculate OCV given a singular value decomposition (SVD) decomposition of data matrix X
//'
//' @param X Data matrix
//' @param y Column matrix of responses
//' @param lambda Numeric hyperparameter controlling the strength of the L2 penalisation (non-negative)
//' @param U Matrix U from SVD of X = UDV
//' @param s Elements of diagonal matrix D from SVD of X = UDV
//' @return Numeric OCV
//' @export
// [[Rcpp::export]]
double get_ocv(arma::mat X, arma::mat y, double lambda, arma::mat U, arma::vec s){
  double p = X.n_cols;
  double n = X.n_rows;
  arma::vec d = pow(s, 2)/(pow(s, 2) + lambda);
  
  //computing A directly
  // arma::mat A = U * arma::diagmat(d) * U.t();
  // arma::vec Ad = arma::diagvec(A);
  // arma::vec mu_hat =  A * y;
  
  //or avoiding storing A
  arma::vec Ad = arma::diagvec(U * arma::diagmat(d) * U.t());
  arma::vec mu_hat = U * arma::diagmat(d) * U.t() * y;
  
  double out = sum(pow(y - mu_hat, 2)/pow(1 - Ad, 2))/n;
  return out;
}


//' Find the optimal regularisation parameter through optimised leave one out cross validation
//'
//' @param X Data matrix
//' @param y Column matrix of responses
//' @param lams Vector of regularisation parameters to test
//' @return Vector of OCVs
//' @export
// [[Rcpp::export]]
arma::vec optim_rr(arma::mat X, arma::mat y, arma::vec lams){
  //  SVD decomposition of X
  arma::mat U, V;
  arma::vec s;
  arma::svd_econ(U, s, V, X, "left");

  // calculate OCV for each lambda model
  double n = lams.n_elem;
  arma::vec out(n);
  for (int i = 0; i < n; i++) {
    double lambda = lams[i];
    out[i] = get_ocv(X, y, lambda, U, s);
  }
  return out;
}


//' Fit a ridge regression model to multiple groups in parallel
//'
//' @param X Data matrix
//' @param y Column matrix of responses
//' @param lams Vector of regularisation parameters to test
//' @param idx Vector of sample groups 
//' @return List with two objects
//' lambdas A vector of the optimal value of lambda for each group
//' betas A matrix where columns are the fitted regression coefficients for each group 
//' @export
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
    arma::uword lam_id = ocvs.index_min(); 
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


//' Predict new samples using the results from par_reg
//'
//' @param X Data matrix of test samples
//' @param betas Matrix of regression coefficients
//' @param idx Vector of sample groups, corresponding to the columns of betas
//' (e.g. idx=c(1, 3) means betas[,1] will be used to predict X[1,], and betas[,3] will be used to predict X[2,])
//' @return Vector of fitted values
//' @export
// [[Rcpp::export]]
arma::vec predict_groups(arma::mat X, arma::mat betas, arma::vec idx){
  
  int n=X.n_rows;
  int g=betas.n_cols;
  arma::vec groups = unique(idx);
  arma::vec y(n);
  
  for(int i=0; i<g; i++)
  {
    // subset the data
    int tod = groups[i];
    arma::uvec rows = find(idx == tod);
    arma::mat X_sub = X.rows(rows);
    arma::vec beta = betas.col(i);

    // predict the samples 
    arma::vec y_sub = predict_rr(X_sub, beta);
    y.rows(rows) = y_sub;
  }

  return y;
}

