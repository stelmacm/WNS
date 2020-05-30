#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  
  DATA_VECTOR(yobserved);
  DATA_MATRIX(mixedmodelx);
  DATA_SPARSE_MATRIX(Z);
  DATA_VECTOR(offset);
  
  PARAMETER_VECTOR(beta);
  PARAMETER_VECTOR(b);
  PARAMETER(re_logsd);
  
  Type jnll=0;
  
  int nobs = yobserved.size();  // number of observations
  vector<Type> mu(nobs);
  
  // Linear predictor
  vector<Type> eta = mixedmodelx * beta + Z * b + offset;
    
    // assume cloglog link
  for (int i=0; i<nobs; i++) {
    mu(i) = Type(1) - exp(-exp(eta(i)));
    // subtract neg log likelihood
    jnll -= dbinom(yobserved(i), Type(1), mu(i), true);
  }
  
  // NLL of random effects
  for( int i=0; i<b.size(); i++){
    jnll -= dnorm(b(i), Type(0.0), exp(re_logsd), true);
  }
  
  return jnll;
  }
