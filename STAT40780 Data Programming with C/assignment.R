
# STAT40780 Data Programming with C
# Assignment
#NAME: Brian Buckley
#STUDENT NUMBER: 14203480


setwd("C:/Users/buckleyb/Documents/Personal/Courses/UCD Data Analytics/STAT40780 Data Prog with C/Code")

#QUESTION 1: The .C interface
#Test whether two vectors are orthogonal

dyn.load("isOrth.dll")

# Test 1: Non-orthogonal vectors
resF <- .C("isOrth", x = as.integer(c(1,2,3,4,5)), y = as.integer(c(1,2,3,4,5)), len = as.integer(5), result = FALSE)
resF

# Test 2: Orthogonal vectors
resT <- .C("isOrth", x = as.integer(c(6, -2, -1)), y = as.integer(c(2, 5, 2)), len = as.integer(3), result = FALSE)
resT

# R Wrapper function.  This contains sanity checks before calling C++
isOrth <- function (x, y) {
    
    if( !(is.integer(x) & is.integer(y))){
        stop("input arguments must be integers")
    }
    
    if((length(x) <= 1) | (length(y) <= 1)){
        stop("input arguments must be vectors")
    }
    
    if(length(x) != length(y)){
        stop("input vectors must be the same size")
    }
    
    return ( .C("isOrth", x = x, y = y, len=length(x), result = FALSE )$result )
}

# call C++ through the wrapper function with tests covering all sanity tests
# and orthogonal and non-orthogonal tests

# Test 1: not integer vectors (R vectors are numeric by default)
x = c(1,2,3,4,5)
y = c(1,2,3,4,5)
isOrth(x, y)

# Test 2: Not a vector
x = as.integer(c(1))
y = as.integer(c(1))
isOrth(x, y)

# Test 3: Different sized vectors
x = as.integer(c(1,2,3,4,5))
y = as.integer(c(1,2,3,4))
isOrth(x, y)

# Test 4: Non-orthogal vectors
x = as.integer(c(1,2,3,4,5))
y = as.integer(c(1,2,3,4,5))
isOrth(x, y)

# Test 5: Orthogonal vectors
x = as.integer(c(6, -2, -1))
y = as.integer(c(2, 5, 2))
isOrth(x, y)

dyn.unload("isOrth.dll")

#QUESTION 2
#WELFORD'S ALGORITHM FOR COMPUTING VARIANCE

library(Rcpp)
library(inline)

body_varCpp <- '
NumericVector xx(x);
double variance = 0;
int n = xx.size();

double M_k = xx[0]; // first element of xx(k=1)
double SSD_k = 0; // because (xx[0] - M_k)^2 = 0

for (int k=1; k<n; k++) {
    double prevM_k = M_k;
    double prevSSD_k = SSD_k;
    M_k = prevM_k + (xx[k] - prevM_k) / k;
    SSD_k = prevSSD_k + (xx[k] - prevM_k)*(xx[k] - M_k);
}

variance = SSD_k/(n-1);

return(wrap(variance));
'

varCpp <- cxxfunction(signature(x = "numeric"),
                       body = body_varCpp,
                       plugin = "Rcpp")

# Use a very large vector to ensure the C++ functions are compared and the 
# result is not masked by R checking code in var()
input_x <- rnorm(1000000)
varCpp(input_x)
var(input_x)

library(rbenchmark)
benchmark(varCpp(input_x), var(input_x), order = "relative")


#QUESTION 3
#Part 1
#Rcpp quicksort function

body_sortCpp <- '
#include <list>
using namespace std;

// Declare the quick sort function as we are putting it in a separate include block
void sort(NumericVector&, int, int);

// xx holds the unsorted vector
NumericVector xx(x);

// Put the unsorted vector onto our return std::list
list<NumericVector> ret;
ret.push_back(xx);

// Clone the input so that we can sort a copy of the vector
NumericVector yy = clone(x);

// Set the bounds of the vector to compute the pivot point
int left= 0;
int right = yy.size() - 1;

// call the quick sort algorithm
sort(yy, left, right);

// add the sorted vector to our list and return it to R
ret.push_back(yy);
return(wrap(ret));
'

# separate the actual recursive quick sort algorithm into an include block
incl <- '
// This quicksort algorithm is explained here:- https://en.wikipedia.org/wiki/Quicksort

void sort(NumericVector& yy, int left, int right) {
    double i = left, j = right; // we need to copy left and right as we will change them
    double tmp; // we ned a temporary holder to perform the swap operation

    // choose a pivot point in the middle of the vector
    double pivot = yy[(left + right)/2]; 
 
    // The Partition Stage
    while (i <= j) {
        while (yy[i] < pivot) {
            i++;
        }
            
        while (yy[j] > pivot) {
            j--;
        }
            
        if (i <= j) {
            tmp = yy[i];
            yy[i] = yy[j];
            yy[j] = tmp;
            i++;
            j--;
        }
    };
 
    // The Recusion stage - sort the left and right sub-vectors
    if (left < j) {
        sort(yy, left, j);
    }
            
    if (i < right) {
        sort(yy, i, right);
    }
}
'
library(Rcpp)
library(inline)

sortCpp <- cxxfunction(signature(x = "numeric"),
                      body = body_sortCpp,
                      include = incl,
                      plugin = "Rcpp")

# Test 1: Sort integers
input_x<-c(64, 3, 45, 3, 11, 9, 9, 3, 34, 31, 31)
ret<-sortCpp(input_x)
ret

# Test 2: Sort numerics
input_x<-c(64.3, 45, 3, 3.5, 11, 9, 9, 3, 34, 31.6, 31,2)
ret<-sortCpp(input_x)
ret

# Test 3: Sort iris data
data(iris)
ret<-sortCpp(iris$Sepal.Length)
ret


#QUESTION 3
#Part 2
#Rcpp quicksort function modified to handle missing values

body_sortCpp2 <- '
#include <list>
using namespace std;

// Declare the quick sort function as we are putting it in a separate include block
void sort(NumericVector&, int, int);

// xx holds the unsorted vector
NumericVector xx(x);

// Put the unsorted vector onto our return std::list
list<NumericVector> ret;
ret.push_back(xx);

// Clone the input so that we can sort a copy of the vector
NumericVector yy = clone(x);

// Remove any NAs before sorting
int naCount = 0;
for (int i=0; i<yy.size(); i++) {
  if (NumericVector::is_na(yy[i])) {
    yy.erase(i);
    naCount++;
  }
}

// Set the bounds of the vector to compute the pivot point
int left= 0;
int right = yy.size() - 1;

// call the quick sort algorithm
sort(yy, left, right);

// Add the removed NAs at the end of the sorted vector
if (naCount > 0) {
    for (int i=0; i<naCount; i++) {
        yy.push_back(NA_REAL);
    }
}

// add the sorted vector to our list and return it to R
ret.push_back(yy);
return(wrap(ret));
'

library(Rcpp)
library(inline)

sortCpp2 <- cxxfunction(signature(x = "numeric"),
                       body = body_sortCpp2,
                       include = incl,
                       plugin = "Rcpp")

x <- round(rnorm(20), 2)
x[sample(1:20, size=3,)]<-NA
x

ret<-sortCpp2(x)
ret







