#include <TMB.hpp>
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
  for(int i = 0; i < numberofyears; i++){
    //Extracting shared users for a given year LOOK INTO BLOCK ARGUEMENTS AGAIN
    matrix<Type> currentsharedusers = SM.block(0 , i*dim, dim , dim); //Block from 0th row 548*(i-1) col taking 548 row and 548 col
    //Block starting at (0,0) taking 548 rows, 548 cols
    
    matrix<Type> suplusdist = (rho * currentsharedusers) + ((1-rho) * weightedmatrix);
    //Extracting incidence for a given year LOOK INTO IF ITS 1XN OR NX1
    matrix<Type> incidenceofyear = fullcountyincidence.block(0, i, dim, 1); //block from 0th row and ith col taking 548 row and just that col. 
    matrix<Type> FOIforayear = suplusdist * incidenceofyear;
    //Makes sense so far
    //FAKE REVELATION: I can use STD libarary
    FOI.col(i) = (FOIforayear.col(0));
  }
  
  //Did this inside of for loop but it would crash R upon further inspection
  vector<Type> foipt1(dim*2);
  foipt1 << FOI.col(0), FOI.col(1);
  
  vector<Type> foipt2(dim*3);
  foipt2 << foipt1, FOI.col(2);
  
  vector<Type> foipt3(dim*4);
  foipt3 << foipt2, FOI.col(3);
  
  vector<Type> foipt4(dim*5);
  foipt4 << foipt3, FOI.col(4);
  
  vector<Type> foipt5(dim*6);
  foipt5 << foipt4, FOI.col(5);
  
  vector<Type> foipt6(dim*7);
  foipt6 << foipt5, FOI.col(6);
  
  vector<Type> foipt7(dim*8);
  foipt7 << foipt6, FOI.col(7);
  
  vector<Type> foipt8(dim*9);
  foipt8 << foipt7, FOI.col(8);
  
  vector<Type> foipt9(dim*10);
  foipt9 << foipt8, FOI.col(9);
  
  vector<Type> foipt10(dim*11);
  foipt10 << foipt9, FOI.col(10);
  
  vector<Type> foipt11(dim*12);
  foipt11 << foipt10, FOI.col(11);
  
  vector<Type> foipt12(dim*13);
  foipt12 << foipt11, FOI.col(12);
  
  //Now need to lag FOI
  //Not sure if there is a nice way to do this so
  //This breaks TMB
  vector<Type> laggedFOI(foipt12.size() + 1);
  laggedFOI.setZero();
  for(int i = 0; i < foipt12.size(); i++){
    laggedFOI[i + 1] = foipt12[i];
  }
  
  Type randomval = sum(laggedFOI);
  
  return randomval;
}