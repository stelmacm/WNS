#include <TMB.hpp>

//Creating special struct for list of sparse matrices (for shared users mat)

// Corresponding list object on the C++ side
template<class Type>
struct my_list {
  vector<Type> a;
  matrix<Type> b;
  my_list(SEXP x){ // Constructor
    a = asVector<Type>(getListElement(x,"a"));
    b = asMatrix<Type>(getListElement(x,"b"));
  }
};

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRUCT(object, my_list);
  REPORT(object.a); // Now you can use "a" and "b" as you like
  REPORT(object.b);
  return 0;
}
