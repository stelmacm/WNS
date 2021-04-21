#include <TMB.hpp>
template<class Type>
Type objective_function <Type>::operator() () {
  DATA_VECTOR(density);
  DATA_VECTOR(surv);
  DATA_MATRIX(X_logit_a);
  DATA_SPARSE_MATRIX(Z_logit_a);
  PARAMETER_VECTOR(logit_a_fixed_param); // fixed effects
  PARAMETER_VECTOR(logit_a_rand_param); // random effects
  PARAMETER(logit_a_logsd);
  PARAMETER(log_h);

  Type nll = 0.0;

  // fixed-effect computation
  vector<Type> logit_a = X_logit_a * logit_a_fixed_param;
  // add random effects to predictor
  logit_a += exp(logit_a_logsd) * (Z_logit_a * logit_a_rand_param);
  // back-transform predictors
  vector<Type> a = invlogit(logit_a);
  Type h = exp(log_h);
  // nonlinear formula computation

  nll -= sum(dbinom(surv, density, vector<Type>(a/(1 + a*h*density)), true));

  // penalization for random effect deviations from 0
  nll -= sum(dnorm(logit_a_rand_param, Type(0), Type(1), true));

  //for (int i=0; i<surv.size(); i++) {
  //     nll = -sum(dbinom_robust(surv[i], density[i], exp(log_a[i])/(1 + exp(log_a[i])*h[i]*density[i]), true));
  // }

  //REPORT(prob);
  //ADREPORT(prob);

  return nll;
}
