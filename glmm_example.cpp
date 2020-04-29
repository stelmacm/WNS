#include <TMB.hpp>
#include <iostream>
#include <string>

template<class Type>
Type objective_function<Type>::operator() ()
{

  DATA_VECTOR(yobs);
  DATA_MATRIX(X);
  DATA_SPARSE_MATRIX(Z);

  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(b);
  PARAMETER(re_logsd);

  Type jnll=0;

  int nobs = yobs.size();  // number of observations
  vector<Type> mu(nobs);

  // Linear predictor
  vector<Type> eta = X * beta + Z * b;  // + offset?

  // assume cloglog link
  for (int i=0; i<nobs; i++) {
    mu(i) = Type(1) - exp(-exp(eta(i)));
    std::cout << "inside obs loop " << i << " " << eta(i) << " " << mu(i) << std::endl;
    // subtract neg log likelihood
    jnll -= dbinom(yobs(i), Type(1), mu(i), true);
  }

  // NLL of random effects
  for( int i=0; i<b.size(); i++){
    jnll -= dnorm(b(i), Type(0.0), exp(re_logsd), true);
    // debugging statement
    std::cout << "inside random effect loop " << i << " " << b(i) << std::endl;
  }

  return jnll;
}
