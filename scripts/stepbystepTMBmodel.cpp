#include <TMB.hpp>
extern "C" {
  /* See 'R-API: entry points to C-code' (Writing R-extensions) */
  double Rf_logspace_sub (double logx, double logy);
  void   Rf_pnorm_both(double x, double *cum, double *ccum, int i_tail, int log_p);
}
/* y(x) = logit_invcloglog(x) := log( exp(exp(x)) - 1 ) = logspace_sub( exp(x), 0 )
 y'(x) = exp(x) + exp(x-y) = exp( logspace_add(x, x-y) )
 */
TMB_ATOMIC_VECTOR_FUNCTION(
  // ATOMIC_NAME
  logit_invcloglog
  ,
  // OUTPUT_DIM
  1,
  // ATOMIC_DOUBLE
  ty[0] = Rf_logspace_sub(exp(tx[0]), 0.);
,
// ATOMIC_REVERSE
px[0] = exp( logspace_add(tx[0], tx[0]-ty[0]) ) * py[0];
)
  template<class Type>
  Type logit_invcloglog(Type x) {
    CppAD::vector<Type> tx(1);
    tx[0] = x;
    return logit_invcloglog(tx)[0];
  }
template<class Type>
Type objective_function <Type>::operator() () {
  //Import data
  DATA_MATRIX(dist); //distance from county i to j (548x548)
  DATA_INTEGER(dim);
  DATA_MATRIX(SM);
  DATA_INTEGER(numberofyears);
  DATA_MATRIX(fullcountyincidence);
  
  //Import Parameters
  PARAMETER(d);
  PARAMETER(theta);
  PARAMETER(rho);
  PARAMETER_VECTOR(YearRandomEffect);
  PARAMETER_VECTOR(CountyRandomEffect);
  
  Type nll = 0.0;
  Type silly = 1.0;
  
  //Begin my applying azzalini parameter
  matrix<Type> azzalinimat(dim, dim);
  azzalinimat.setZero();
  for(int i = 0; i < dim; i++){
    for(int j=0; j < dim; j++){
      Type currentval = dist(i,j);
      Type hyperparam = -1* pow(currentval/d , theta);
      azzalinimat(i,j) = hyperparam;
    }
  }
  
  matrix<Type> azzaliniexp = exp(azzalinimat.array());
  
  //Now I want to set diag of matrix equal to 0
  //This gets number of elements not size of matrix FIX ME
  for(int i = 0; i < dim; i++) {
    azzaliniexp(i,i) = 0;
  }
  
  //Create row sum 1 weight matrix of azzalini distances
  matrix<Type> weightedmatrix(dim, dim);
  weightedmatrix.setZero();
  for(int i = 0; i < dim; i++){
    vector<Type> currentrow = azzaliniexp.row(i);
    Type sumofvec = currentrow.sum();
    vector<Type> newrow = currentrow / sumofvec;
    //Reattach the weighted vector to the weightedmatrx
    weightedmatrix.row(i) = newrow;
  }
  
  //Now creating
  matrix<Type> FOImat(dim,numberofyears);
  FOImat.setZero();
  vector<Type> FOI(dim*numberofyears);
  for(int i = 0; i < (numberofyears - 1); i++){
    //Extracting shared users for a given year LOOK INTO BLOCK ARGUEMENTS AGAIN
    matrix<Type> currentsharedusers = SM.block(0 , i*dim, dim , dim); //Block from 0th row 548*(i-1) col taking 548 row and 548 col
    //Block starting at (0,0) taking 548 rows, 548 cols
    
    matrix<Type> suplusdist = (rho * currentsharedusers) + ((1-rho) * weightedmatrix);
    //Extracting incidence for a given year LOOK INTO IF ITS 1XN OR NX1
    matrix<Type> incidenceofyear = fullcountyincidence.block(0, i, dim, 1); //block from 0th row and ith col taking 548 row and just that col. 
    matrix<Type> FOIforayear = suplusdist * incidenceofyear;
    //Makes sense so far
    //FAKE REVELATION: I can use STD libarary
    //FOI.col(i) = (FOIforayear.col(0));
    
    matrix<Type> incidenceofnextyearmatrix = fullcountyincidence.block(0, i + 1, dim, 1);
    vector<Type> incidenceofnextyearvec = incidenceofnextyearmatrix.col(0);
    for(int j = 0; j < dim; j++){
      if(fullcountyincidence(j,i) == 0){
        Type linearpred = FOIforayear(j,0) + YearRandomEffect(i) + CountyRandomEffect(j);
        Type prob = logit_invcloglog(linearpred);
        Type incidenceofnextyear = incidenceofnextyearvec(j);
        //incidenceofnextyear = countylist.block(0, i+1, ...)
        nll -= dbinom(incidenceofnextyear, silly, prob, true);
      }
    }
  }
  

  nll -= sum(dnorm(YearRandomEffect, Type(0), Type(1), true));
  nll -= sum(dnorm(CountyRandomEffect, Type(0), Type(1), true));

  // Priors mean changing 
  //nll -= dnorm()
  //nll -= dnorm()
  
  return nll;
}
