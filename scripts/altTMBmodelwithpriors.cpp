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
  Type objective_function <Type>::operator() () {
    //Import data
    DATA_MATRIX(dist); //distance from county i to j (548x548)
    DATA_INTEGER(dim);
    DATA_MATRIX(SM);
    DATA_INTEGER(numberofyears);
    DATA_MATRIX(fullcountyincidence);
    
    // external prior values
    DATA_SCALAR(dpriormean); //on d
    DATA_SCALAR(dpriorscalingparam); //on d
    DATA_SCALAR(thetapriorpower); // on d
    DATA_SCALAR(rhomean);
    DATA_SCALAR(rhostd);
    DATA_SCALAR(offsetmean);
    DATA_SCALAR(offsetsd);
    DATA_SCALAR(thetapriorscale); //on theta
    DATA_SCALAR(thetapowerscale); // on theta
    DATA_SCALAR(thetamean); //on theta 
      
      //Import Parameters
      PARAMETER(log_d);  // e.g. dnorm; range is log(10 km) - log(1000 km), mean in the middle,
    // SD = (max-min)/6 to make this range be +/- 3 SD = 0.997 of the prob
    PARAMETER(theta);  // reasonable range for this is 0.5-3
    // might want to make this log-scale as well;
    // use the same trick as above, i.e. range log(0.5)-log(3)
    PARAMETER(logit_rho);  // logit-normal, not so wide that there are big peaks at 0/1
    // probably mean at 0 (=0.5 proportion)
    // if you set rho ~ U(0,1) this is actually equivalent to
    // not explicitly adding a prior!
    PARAMETER(log_offsetparam); // probably log-normal with range log(0.001)-log(10)?
    PARAMETER(logsd_Year);      // half-Normal? half-t? half-Cauchy?
    // 2*dnorm(exp(logsd_Year), Type(0.0), logsd_Year_sd)
    
    //Confused I think
    PARAMETER(logsd_County);
    // main thing to keep in mind as that the SD here is in log-hazard units
    //  so, think about a 'reasonable' amount of among-year or among-county variation
    //  in log-hazard; so a SD of 5 (i.e. a 95% range of 20) is probably way too big
    
    // the other thing: PRIOR PREDICTIVE SIMULATIONS
    // note there is a SIMULATE() macro in TMB ...
    
    PARAMETER_VECTOR(YearRandomEffect);
    PARAMETER_VECTOR(CountyRandomEffect);
    
    
    Type nll = 0.0;
    // apply inverse link functions
    Type d = exp(log_d);
    Type rho = invlogit(logit_rho);
    Type offsetparam = exp(log_offsetparam);
    
    //Begin my applying azzalini parameter
    matrix<Type> azzalinimat(dim, dim);
    azzalinimat.setZero();
    for(int i = 0; i < dim - 1; i++){
      for(int j= i + 1 ; j < dim; j++){
        Type currentval = dist(i,j); 
        Type hyperparam = -1* pow(currentval/d , theta);
        // BMB: can we exponentiate at this step??
        azzalinimat(i,j) = azzalinimat(j,i) = exp(hyperparam);
      }
    }
    //No longer have to set the diag to 0 bc its already 0
    
    //Create row sum 1 weight matrix of azzalini distances
    matrix<Type> weightedmatrix(dim, dim);
    weightedmatrix.setZero();
    for(int i = 0; i < dim; i++){
      vector<Type> currentrow = azzalinimat.row(i);
      Type sumofvec = currentrow.sum();
      vector<Type> newrow = currentrow / sumofvec;
      //Reattach the weighted vector to the weightedmatrx
      weightedmatrix.row(i) = newrow;
    }
    
    //Now creating
    for (int year = 0; year < (numberofyears - 1); year++) {
      //Extracting shared users for a given year 
      matrix<Type> currentsharedusers = SM.block(0 , year*dim, dim , dim); //Block from 0th row 548*(i-1) col taking 548 row and 548 col
      //Block starting at (0,0) taking 548 rows, 548 cols
      
      matrix<Type> suplusdist = (rho * currentsharedusers) + ((1-rho) * weightedmatrix);
      //Extracting incidence for a given year LOOK INTO IF ITS 1XN OR NX1
      matrix<Type> incidenceofyear = fullcountyincidence.block(0, year, dim, 1); //block from 0th row and ith col taking 548 row and just that col. 
      matrix<Type> FOIforayear = suplusdist * incidenceofyear;
      
      // FIXME: if we do the azzalini matrix calculation and the shared-user calculation
      // *inside* the (if uninfected) block, then we don't have to do any of those calculations
      // for (year, target county) combinations where the county is already infected
      for (int county = 0; county < dim; county++) {
        if (fullcountyincidence(county,year) == 0) {
          
          // compute azzalini distances, scale them, add up shared-users for this
          // county and all other counties in this year, etc.
          
          Type individFOIforayear = FOIforayear(county,0);
          Type logFOIforayear = log(individFOIforayear + offsetparam);
          Type linearpred = logFOIforayear + YearRandomEffect(year) + CountyRandomEffect(county);
          Type logit_prob = logit_invcloglog(linearpred);
          nll -= dbinom_robust(fullcountyincidence(county,year+1), Type(1.0), logit_prob, true);
        } // uninfected counties
      } // loop over counties
    } // loop over years
    
    // technical issue, when we get to Stan-world, that it's better to make the random effects UNSCALED
    // (i.e. random effects are N(0,1), we multiply by sd before adding them to the linear predictor
    // rather than SCALED (i.e. random effects are N(0,sd), we add them directly to the LP)
    // ... we may want to leave these as N(0,1)
    // we could ADREPORT() a scaled version of the random effects, for convenience
    nll -= sum(dnorm(YearRandomEffect, Type(0), exp(logsd_Year), true));
    nll -= sum(dnorm(CountyRandomEffect, Type(0), exp(logsd_County), true));
    
    //Implementing the general exponential distribution
    // nll -= - theta * (x - x_0)/d (because its the log of the azzalini bubble)
    // Trick -> nll -= - theta -> nll += theta
    nll += thetapriorpower * (sqrt(pow(log_d - dpriormean,2))/dpriorscalingparam); //need it to be absolute
    //x0 = mean upr and lwr
    //x = the d scaling param
    //thetaprior = power param from fancy calc
    //dprior scale param from fancy calc
    // x = actual scale param from the model
    
    //Put prior on theta. (also azzalini)
    //get_gnorm(lower=log(0.5), upper=log(4))
    //i.e. azzalini prior on log-theta
    //get_gnorm(lower=0.5, upper=4)
    //work with theta, not log-theta
    nll -= pow(exp(- sqrt(pow(theta - thetamean ,2)) / thetapriorscale), thetapowerscale);
    //Is this right or is it just suppose to be thetapriorpower - theta in the nll above
    
    nll -= dnorm(logit_rho, rhomean, rhostd, true);
    // Gaussian prior on offset
    //This one will be tricky because the range is so big for it!
    nll -= dnorm(offsetparam, offsetmean, offsetsd, true);
    
    nll -= dlnorm(YearRandomEffect, Type(0), exp(logsd_Year), true);
    nll -= dlnorm(CountyRandomEffect, Type(0), exp(logsd_County), true);
    return nll;
  }
  //(x < 0 ? -1*x : x ) <- find in cppAD
  //CppAD::CondExpLt {Lt, Le, Gt, Ge} 