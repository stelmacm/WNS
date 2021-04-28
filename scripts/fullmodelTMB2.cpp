#include <TMB.hpp>
//I was today years old when I learned I could use C++ packages
//Jokes you can't

//This time instead of having a list of matrices, will just have 1 big matrix
//And will subset what I want from each 

template<class Type>
Type objective_function<Type>::operator() ()
{
  //Import Data
  DATA_INTEGER(dim);
  DATA_INTEGER(numberofyears);
  DATA_MATRIX(SM); //Shared user matrix rows:(548 x 12 (years)) cols: 548
  DATA_MATRIX(dist); //distance from county i to j (548x548)
  DATA_MATRIX(countylist); //548 row (counties) 13 col (per year) incidence matrix
  DATA_VECTOR(uninfectedcounty); //uninfected counties post 2006
  DATA_FACTOR(yearindicator);
  DATA_FACTOR(countyindicator);
  DATA_FACTOR(incidence); //What we are ultimately trying to predict
  //Import Parameters
  PARAMETER(d);
  PARAMETER(theta);
  PARAMETER(rho);
  PARAMETER(a);
  //PARAMETER_VECTOR(Fixed_vector);
  PARAMETER_VECTOR(Random_vectorone);
  PARAMETER_VECTOR(Random_vectortwo);
  
  Type nll = 0.0;
  
  //Now start the process
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
    vector<Type> currentrow = azzalinimat.row(i);
    Type sumofvec = currentrow.sum();
    vector<Type> newrow = currentrow / sumofvec;
    //Reattach the weighted vector to the weightedmatrx
    weightedmatrix.row(i) = newrow;
  }
  
  //Now we will extract the Shared User matrix for the year in question
  //And we will multiply it by the weighted matrix and then by incidence
  //matrix<Type> FOI(dim, numberofyears);
  vector<Type> FOI; //Uninitialized vector. Scary. DO SOMETHING ABOUT THIS
  for(int i = 0; i < numberofyears; i++){
	  // BMB: only go as far as i < numberofyears-1?
	  // (we don't need to calculate FOI for the last year ...
    //Extracting shared users for a given year LOOK INTO BLOCK ARGUEMENTS AGAIN
    matrix<Type> currentsharedusers = SM.block(0 , i*dim, dim , dim); //Block from 0th row 548*(i-1) col taking 548 row and 548 col
    //Block starting at (0,0) taking 548 rows, 548 cols
    
    matrix<Type> suplusdist = (rho * currentsharedusers) + ((1-rho) * weightedmatrix);
    //Extracting incidence for a given year LOOK INTO IF ITS 1XN OR NX1
    matrix<Type> incidenceofyear = countylist.block(0, i, dim, 1); //block from 0th row and ith col taking 548 row and just that col. 
    //Does this only make 12 col instead of 13???
    matrix<Type> FOIforayear = suplusdist * incidenceofyear;
    // BMB

    //   incidenceofnextyear = countylist.block(0, i+1, ...)
    //    for (int j = 0; j < numberofcounties; j++) {
    //       if (county(j) already infected in year i -> skip to next iteration
    //         (i.e. if (incidenceofyear(j,0)==1))
    //       otherwise ...
    //        calculate log-likelihood of incidenceofnextyear(j) given FOIforayear(j,0)
    //    linearpred = FOIforayear(j,0) + Randomeffectone(i) + Randomeffecttwo(j)
    //    prob = invcloglog(linearpred)
    //    nll -= dbinom(incidenceofnextyear(j), 1, prob, true) or whatever
    //        and subtract from the running nll counter

    //  you can dbinom(..., prob=invcloglog(linearpred)
    // OR
    //          dbinom_robust(..., prob=glmmtmb::logit_invcloglog(linearpred))
    // penalizations OUTSIDE OF LOOP
    //  can steal code from https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/src/distrib.h#L186-L214 if you want 
    
    //Makes sense so far
    //FAKE REVELATION: I can use STD libarary
    vector<Type> FOIplaceholder = (FOIforayear.col(0));
    FOI << FOI, FOIplaceholder; //I hope this is okay
  }
  
  //Now need to lag FOI
  //Not sure if there is a nice way to do this so
  //Cheap fix
  vector<Type> laggedFOI(FOI.size() + 1);
  laggedFOI.setZero();
  for(int i = 0; i < FOI.size(); i++){
    laggedFOI[i + 1] = FOI[i];
  }
  //COME BACK TO THIS
  
  //Now need to remove every 2006 (Should be every thirteenth row)
  
  //Now will multiply each column of FOI (each year's Force of infection) times the uninfected counties (ie removes FOI once county
  //has already become infected)
  vector<Type> FOIofuninf = laggedFOI * uninfectedcounty; //Elementwise multiplication (turns already infected counties to 0)
  
  vector<Type> firstyearremoved(dim*(numberofyears - 1));
  for(int i = 0; i < FOIofuninf.size(); i++){
    if((i%numberofyears) == 0){
      firstyearremoved << firstyearremoved, FOIofuninf(i);
    }
  }
  //This doesnt work because there is no modulus of an integer
  
  
  //Now I need to remove counties once they are infected
  //I think I'm defeating the purpose of the lag
  //Remove using if maybe
  vector<Type> finalFOI;
  for(int i = 0; i < firstyearremoved.size(); i++){
    if(firstyearremoved(i) != 0){
      finalFOI << finalFOI, firstyearremvoved(i);
    }
  }


  
  //This actually might fuck up if FOI is super super small
  
  //Now need to import a perfect set of factored years and factored counties
  //These will be used for random effects
  
  // fixed-effects (Still need to do offset param)
  vector<Type> logit = log(finalFOI + a);

  // BMB do all of this stuff one county/year at a time
  // add random effects to predictor
  logit += (yearindicator * Random_vectorone);
  logit += (countyindicator * Random_vectortwo); //Do I need 2 random vectors? Or just 1
  // back-transform predictors (I'm lost)
  vector<Type> val = invlogit(logit);
  //Type h = exp(log_h); //I dont think I need this
  // nonlinear formula computation
  //dbinom(# successes, # trails, probability)
  nll -= sum(dbinom(incidence, finalFOI.size(), val , true));
  
  // penalization for random effect deviations from 0
  //Penalize twice to make them nice
  nll -= sum(dnorm(Random_vectorone, Type(0), Type(1), true));
  nll -= sum(dnorm(Random_vectortwo, Type(0), Type(1), true));
  
  return nll;
}
