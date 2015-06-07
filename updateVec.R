library(Rcpp)
library(inline)


rcpp_inc = '
using namespace Rcpp;
using namespace arma;
'

#matrix computation function in c++
src = '
mat subm = as<mat>(submin);
mat e = as<mat>(ein);
vec rate = as<vec>(ratein);
double lambda = as<double>(lambdain);

mat m1 = subm * trans(subm) + lambda * rate.size() * e;
mat m2 = subm * mat(rate);
mat out = inv(m1) * m2;
return(wrap(out));
'

#R wrap up
updateVec = cxxfunction(signature(submin="numeric",ein="numeric",ratein="numeric",lambdain="numeric"),
                        src, plugin='RcppArmadillo',rcpp_inc)
