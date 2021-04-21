//This will be the individual model C code that will be used to create the model made for optim
//Defintely going to have alot of problems such as melting or weighting a matrix or what not
//https://github.com/kaskr/adcomp/issues/96
#include <TMB.hpp>

template<class Type>
struct my_list {
  matrix<Type> a;
  matrix<Type> b;
  my_list(SEXP x){ // Constructor
    a = asMatrix<Type>(getListElement(x,"a"));
    b = asMatrix<Type>(getListElement(x,"b"));
  }
};


template<class Type>
Type objective_function<Type>::operator() ()
{
  //Import Data Matrices
  //Consider using it in form of data struct rather than data array
  //ALTERNATIVE. Make big matrix and then "extract" matrix that I want (could use matrix info as params)
  //SOLUTION: Link above shows how to keep them as structs
  DATA_STRUCT(SM, my_list);
  DATA_MATRIX(dist);
  DATA_STRUCT(countylist, my_list); //Countylist isn't actually a sparse matrix so maybe I need to consider that
  //Import Parameters
  PARAMETER(d);
  PARAMETER(theta);
  PARAMETER(rho);
  PARAMETER(a);
  
  //Now start the process
  //Begin my applying azzalini parameter
  
  matrix<Type> azzalinimat = exp(-1*(dist.array() * (1/d))^theta); // I mean this already makes me say yikes
  //Now I want to set diag of matrix equal to 0
  int dim = azzalini.row(); //This gets number of elements not size of matrix FIX ME
  for(int i = 0; i < dim; i++) {
    azzalinimat(i,i) = 0;
  }
  //Create weight matrix of azzalini distances
  matrix<Type> weightedmatrix(dim, dim);
  for(int i = 0; i < dim; i++){
    vector<Type> currentrow = azzalinimat.row(i);
    int sumofvec = currentrow.sum();
    vector<Type> newrow = currentrow / sumofvec;
    //Reattach the weighted vector to the weightedmatrx
    weightedmatrix.row(i) = newrow;
  }
  
  //Now going to multiply weightmatrix by the shared user matrix and then going to multiply it by dist
  //in R code this is for i in levels(years)...Need to find creative way to do that 
  //Defining matrix outside of forloop 
  matrix<Type> forceofinfection(dim, dim);
  
  for(int i = 0; i < 13; i++){
    //This is where we take apart the arrays (per year matrices)
    //Multiplying rho times shared users (for a given year) plus (1-rho) times distance mat
    matrix<Type> usertimeslocation = rho * SM.matrix(i) + (1-rho)*dist;
    //Now multiply usertimeslocation by countyincidence per year
    //Store as vector and then we will make a mmatrix of it
    //Wonder if I can store 1xn matrix as vector
    matrix<Type> individualfoivector = usertimeslocation * countylist.matrix(i);
    //Doing some cheeky manipulation I think
    vector<Type> foivectorplaceholder = individualfoivector.row(0);
    //I think I want this as 
    forceofinfection.col(i) = foivectorplaceholder;
    //I am very positive there is a more clever way to do this
  }
    
  //Now I need to melt the matrix. Maybe I should have kept it in a more reasonable format
  //so I could have created the model
  
  //Also need to left join incidence df with FOI df
  
  //Need to subset things. Need a better way to do these things
  
  //Need to actually create the mixed model
  
  
  //Calculate negative log lik
  return nll;
}