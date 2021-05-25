# RcppRidge Package
This package was developed for a group project (Bristol Compass CDT), to perform Bayesian ridge regression using Rcpp and parallel programming. The package is demonstrated on an electricity demand dataset.

### Development

Package created using:

`Rcpp::Rcpp.package.skeleton("RcppRidge")`

`usethis::use_rcpp()`

`usethis::use_test("RcppRidge")`


To build the package:

`Rcpp::compileAttributes()`

`system("R CMD build")`

`system("R CMD INSTALL ../RcppRidge")`


To update the documentation:

`roxygen2::roxygenize(roclets="rd")`

`system("R CMD Rd2pdf --pdf --title='RcppRidge Package Documentation' -o RcppRidge_Documentation.pdf man/*.Rd")`

On Linux you may need to install the following packages for the above to work

`sudo apt-get install texinfo texlive-fonts-extra`

### Testing

Run the tests 

`devtools::test()`

### Usage

To load the package:

`devtools::install_github("g-l-mansell/RcppRidge")`

`library(RcppRidge)`
