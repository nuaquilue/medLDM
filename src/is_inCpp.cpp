#include <Rcpp.h>
//#include <RcppArmadillo.h>

using namespace Rcpp;

// A iterative binary search function. It returns 
// location of x in given array arr[l..r] if present, 
// otherwise -1 

// [[Rcpp::export]]
int binarySearch(NumericVector arr, int l, int r, int x) 
{ 
    while (l <= r) { 
        int m = l + (r - l) / 2; 
  
        // Check if x is present at mid 
        if (arr[m] == x) 
            return (m+1); 
  
        // If x greater, ignore left half 
        if (arr[m] < x) 
            l = m + 1; 
  
        // If x is smaller, ignore right half 
        else
            r = m - 1; 
    } 
  
    // if we reach here, then element was 
    // not present 
    return -1; 
} 
  

// [[Rcpp::export]]
NumericVector is_inCpp(NumericVector x, NumericVector y){
  int len_x = x.size();
  int len_y = y.size();
  NumericVector z(len_x);
  for (int i=0; i<len_x; i++){
    z[i] = binarySearch(y, 0, len_y-1, x[i]);
  }  
  return z; 
}