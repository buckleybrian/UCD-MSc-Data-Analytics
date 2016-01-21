#NAME: Brian Buckley
#STUDENT NUMBER: 14203480

setwd("C:/Users/buckleyb/Documents/Personal/Courses/UCD Data Analytics/STAT40780 Data Prog with C/Code")


#QUESTION 1: The .C interface
#(c) R Wrapper function

dyn.load("fibC.dll")

# Test 1: does fibC.cpp work
fibSeq <- .C("fibC", n = as.integer(30), vec = as.integer(0))
fibSeq

# R Wrapper function.  This contains sanity checks before calling C++
fibSeq <- function (x, vec) {
    
    if( !is.numeric(x)){
        stop("input argument n must be numeric")
    }
    
    if(x < 1){
        stop("input argument n must be equal to or greater than 1")
    }
    
    return ( .C("fibC", n = as.integer(x), vec = as.integer(0))$vec )
}

fibSeq(10, 0)

dyn.unload("fibC.dll")

#Question 2

library(Rcpp)
library(inline)
library(rbenchmark)

#(a)
body_traceC <- '
    NumericMatrix xx(x);
    if(xx.ncol() != xx.nrow()) {
        stop("input should be a square matrix");
    }

    // Find the trace of the matrix
    int trace = 0;
    for(int i=0;i<xx.nrow();i++){
        for(int j=0;j<xx.ncol();j++){
            if(i==j){
                trace = trace + xx[i,j];
            }
        }
    }
    return wrap(trace);         
'

traceC <- cxxfunction( signature(x = "numeric"),
                          body = body_traceC,
                          plugin= "Rcpp")

X <- matrix (1:1000000, nrow=1000)
traceC(X)

// Pass non-square matrix
traceC(X[,1:2])

#(b)

benchmark(traceC(X), sum(diag(X)), order = "relative")

#(c)

x <- rnorm(1000, mean = 2, sd = 2.5)

body_empRuleC <- '
 NumericVector xx(x);
 double mean = as<double>(y);
 double sd = as<double>(z);
 
// 1 sd from the mean
int oneSd = 0;
for (int i=0; i<xx.size(); i++) {
    if (xx[i] + sd < mean && xx[1] - sd > mean) {
        oneSd++;
    }
}

// 2 sd from the mean
int twoSd = 0;
for (int i=0; i<xx.size(); i++) {
    if (xx[i] + (2*sd) < mean && xx[1] - (2*sd) > mean) {
        twoSd++;
    }
}

// 3 sd from the mean
int threeSd = 0;
for (int i=0; i<xx.size(); i++) {
    if (xx[i] + (3*sd) < mean && xx[1] - (3*sd) > mean) {
        threeSd++;
    }
}

double percent = ((oneSd + twoSd + threeSd)/ xx.size()) * 100;

 return wrap(percent);
'

EmpRuleC <- cxxfunction( signature(x = "numeric", y="numeric", z="numeric"),
                       body = body_empRuleC,
                       plugin= "Rcpp")


x
EmpRuleC(x, 0.5, 0.3)


#Question 3
#(b)

inc <-'
#include <algorithm>

'
body_mysteryFun <- '

 NumericVector xx(x);
int k = as<double>(K);

// Use the std::qsort 
double result = std::qsort(xx, xx.size() -1, sizeof(k));

return wrap(result);

'

#(c)

body_percentileRcpp <- '
 NumericVector xx(x);
 double pp = as<double>(p);

// remove missing values
 LogicalVector y = is_na(xx);
 NumericVector yy = clone(x);
 for (int i=0; i<xx.size(); i++) {
    if (!y[i]) {
        yy[i] = xx[i];
    }
 }

 // call the R percentileR function
 int result = Function fun(percentileR, yy, pp);
 return wrap(result);
'

percentileRcpp <- cxxfunction( signature(x = "numeric", p="numeric"),
                         body = body_percentileRcpp,
                         plugin= "Rcpp")
















