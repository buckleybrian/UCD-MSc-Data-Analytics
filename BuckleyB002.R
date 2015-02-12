# Brian Buckley 14203480

# STAT40730 Assignment 2

## Task 1: statistical modelling ##

### 1. A function which takes a binary response variable y and a single explanatory variable x (factor or numeric),
###   runs 10-fold cross-validation and returns the proportion of the response variables y that are incorrectly classified
###   i.e. the misclassification rate

logisticSingle = function (y, x) {
    
    #Create 10 equally size folds
    folds<-cut(seq(1,length(x)),breaks=10,labels=FALSE)
    
    # A list to hold each fold's prediction
    preds<-vector()
    
    #Perform 10 fold cross validation
    for(i in 1:10) {
        #Segement the data by fold using the which() function 
        idx<-which(folds==i,arr.ind=TRUE)
        test<-x[idx]
        train<-x[-idx]
        
        # ensure y length is the same as train length for glm()
        ytrain<-y[-idx]
                 
        # Fit the glm model on the training set
        glm.model<-glm(ytrain ~ train, family="binomial")
        
        # Use the fitted model on the test set
        # check whether test is composed of factors or numerics
        xgrid = 0
        if (is.factor(test)) {
            # convert to numeric levels
            xgrid = seq(levels(test))
        } else {
            xgrid = seq(min(test),max(test),by=1)
        }
        
        pred<-predict(glm.model, data.frame(train = test), type='response')
        
        # use a threshold of 0.5 to round the probability into a binary predictor - 1 (>= 0.5) or 0 (< 0.5)
        predbinary<-vector()
        for (i in 1:length(pred)) {
            if (pred[i] >= 0.5) {
                predbinary[i]<-1
            } else {
                predbinary[i]<-0
            }
        }
        
        # combine the predictions from all 10 folds
        preds<-c(preds, predbinary)  
    }
    
    # Compute the misclassification rate - use table()
    confmatrix = table(y, preds)
    
    # add false positives and false negatives together and divide by the total data length to get the 
    # missclassification rate as a proportion
    totals = 1
    if (ncol(confmatrix) == 2) {
        totals = confmatrix[1,2] + confmatrix[2,1]
    } else {
        totals = confmatrix[2,1]
    }
    
    mcr = totals/length(y)
    
    return(mcr)  
 }  

### 2. Use your function on the birthweight data to assess which of the explanatory variables 
###   performs best at predicting low birth weight i.e. has the lowest misclassification rate

library(MASS)
str(birthwt)

## 'data.frame':    189 obs. of  10 variables:
## $ low  : int  0 0 0 0 0 0 0 0 0 0 ...
## $ age  : int  19 33 20 21 18 21 22 17 29 26 ...
## $ lwt  : int  182 155 105 108 107 124 118 103 123 113 ...
## $ race : int  2 3 1 1 1 3 1 3 1 1 ...
## $ smoke: int  0 0 1 1 1 0 0 0 1 1 ...
## $ ptl  : int  0 0 0 0 0 0 0 0 0 0 ...
## $ ht   : int  0 0 0 0 0 0 0 0 0 0 ...
## $ ui   : int  1 0 0 1 1 0 0 0 0 0 ...
## $ ftv  : int  0 3 1 2 0 0 1 1 1 0 ...
## $ bwt  : int  2523 2551 2557 2594 2600 2622 2637 2637 2663 2665 ...
 
# The response variable is $low and the actual birth weight is $bwt 
#  so we use $age, $lwt, $race, $smoke, $ptl, $ht, $ui, $ftv as the list of explnatory variables to compare

names<-c("age", "lwt", "race", "smoke", "ptl", "ht", "ui", "ftv")

mcrs<-c(logisticSingle(birthwt$low, birthwt$age),
logisticSingle(birthwt$low, birthwt$lwt),
logisticSingle(birthwt$low, birthwt$race),
logisticSingle(birthwt$low, birthwt$smoke),
logisticSingle(birthwt$low, birthwt$ptl),
logisticSingle(birthwt$low, birthwt$ht),
logisticSingle(birthwt$low, birthwt$ui),
logisticSingle(birthwt$low, birthwt$ftv))

results<-data.frame(names, mcrs)

rOrdered<-results[with(results, order(mcrs)),]
print(rOrdered)

# names      mcrs
# 1   age 0.3121693
# 3  race 0.3121693
# 4 smoke 0.3121693
# 8   ftv 0.3121693
# 2   lwt 0.3174603
# 5   ptl 0.3386243
# 6    ht 0.3386243
# 7    ui 0.3809524

# It can be seen that age, race, smoke and ftv all have the same lowest misclassification rate (0.3121693)

### 3. Repeat analysis for the South African heart data 

# read the data from the URL in the provided info file
SAdata = read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data",sep=",",head=T,row.names=1)

str(SAdata)

## 'data.frame':    462 obs. of  10 variables:
## $ sbp      : int  160 144 118 170 134 132 142 114 114 132 ...
## $ tobacco  : num  12 0.01 0.08 7.5 13.6 6.2 4.05 4.08 0 0 ...
## $ ldl      : num  5.73 4.41 3.48 6.41 3.5 6.47 3.38 4.59 3.83 5.8 ...
## $ adiposity: num  23.1 28.6 32.3 38 27.8 ...
## $ famhist  : Factor w/ 2 levels "Absent","Present": 2 1 2 2 2 2 1 2 2 2 ...
## $ typea    : int  49 55 52 51 60 62 59 62 49 69 ...
## $ obesity  : num  25.3 28.9 29.1 32 26 ...
## $ alcohol  : num  97.2 2.06 3.81 24.26 57.34 ...
## $ age      : int  52 63 46 58 49 45 38 58 29 53 ...
## $ chd      : int  1 1 0 1 1 0 0 1 0 1 ...

# The respponse variable is $chd

names<-c("sbp", "tobacco", "ldl", "adiposity", "famhist", "typea", "obesity", "alcohol", "age")

mcrs<-c(logisticSingle(SAdata$chd, SAdata$sbp),
        logisticSingle(SAdata$chd, SAdata$tobacco),
        logisticSingle(SAdata$chd, SAdata$ldl),
        logisticSingle(SAdata$chd, SAdata$adiposity),
        logisticSingle(SAdata$chd, SAdata$famhist),
        logisticSingle(SAdata$chd, SAdata$typea),
        logisticSingle(SAdata$chd, SAdata$obesity),
        logisticSingle(SAdata$chd, SAdata$alcohol),
        logisticSingle(SAdata$chd, SAdata$age))

results<-data.frame(names, mcrs)

rOrdered<-results[with(results, order(mcrs)),]
print(rOrdered)

# names      mcrs
# 2   tobacco 0.3008658
# 9       age 0.3246753
# 3       ldl 0.3290043
# 1       sbp 0.3354978
# 4 adiposity 0.3419913
# 6     typea 0.3463203
# 7   obesity 0.3506494
# 8   alcohol 0.3506494
# 5   famhist 0.3852814

# The results show that cumulative tobacco best predicts coronary heart disease from these data

## Task 2: matrix inversion ##

### 1. A useful technique for inverting matrices is that of the block-wise inversion method. 
###    A description can be found at the Wikipedia page: 
###    https://en.wikipedia.org/wiki/Matrix inversion#Blockwise inversion

mInvert <- function(m) {
    # Check that the matrix is square as the block-wise method only works on square matrices
    if (nrow(m) != ncol(m)) {
        return(sprintf("Error: This function only works on a square matrix.  Yours is (%dx%d)", nrow(m), ncol(m))) 
    }
    
    # Check if the matrix is 2x2 and in this case use the analytic method
    if (nrow(m) == 2) {
        return(matrix( matrix(c(m[2,2],-m[2,1],-m[1,2],m[1,1]),2,2),2,2) / ((m[1,1]*m[2,2])-(m[1,2]*m[2,1])) )
    }
    
    # Check if m is a 1 x 1 matrix so that we can break out of the recursion
    # - otherwise we get the error "evaluation nested too deeply: infinite recursion" 
    if(nrow(m) == 1)
    {
        return (matrix(1.0/m[1,1]))
    }
   
    # First split the matrix into blocks using the wikipedia equations for A, B, C and D - see Equation (1)
    # index by dividing the matrix into four quarters
    i<-nrow(m)/2
    A<-m[1:i,1:i,drop=F]
    B<-m[1:i,-1:-i,drop=F]
    C<-m[-1:-i,1:i,drop=F]
    D<-m[-1:-i,-1:-i,drop=F]
    
    # Now recursively call this function for the inverse A block
    Ai<-mInvert(A)
    
    # Recuresively call again for the wikipedia equation for the inverse multiplication term
    # (D-CAiB)i.  Store this in a temp variable to make it easier to deal with later as it occurs
    # in a number of places in equation (1)
    tmpVar<-mInvert(D-C %*% Ai %*% B)
    
    # Compute the square matrix elements using the wikipedia equation (1) 
    topLeft <- Ai + Ai %*% B %*% tmpVar %*% C %*% Ai
    topRight <- -Ai %*% B %*% tmpVar
    lowerLeft <- -tmpVar %*% C %*% Ai
    lowerRight <- tmpVar
    
    # Combine all of the intermediate blocks to arrive at the end result
    result <- cbind(rbind(topLeft, lowerLeft), rbind(topRight, lowerRight))
    return(result)
}

### 3. Test the function

set.seed(100) # ensure pseudo-random output is the same every time
M = matrix(rnorm(2^2),2,2)
M2 = matrix(rnorm(5^2),5,5)
M3 = matrix(rnorm(150^2),150,150)

print("Using mInvert Function:")
print(mInvert(M))
print("Using the R Solve Function:")
solve(M)
print("Test if both are equal")
all.equal(mInvert(M), solve(M))

print("Using mInvert Function:")
print(mInvert(M2))
print("Using the R Solve Function:")
solve(M2)
print("Test if both are equal")
all.equal(mInvert(M2), solve(M2))

print("Using mInvert Function:")
print(mInvert(M3))
print("Using the R Solve Function:")
solve(M3)
print("Test if both are equal")
all.equal(mInvert(M3), solve(M3))

## Task 3: writing S3 methods ##

### 1. Write a print method for the function you created in Task 1. The output
###    should include (at least) the misclassifcation rate and a misclassifcation
###    table of predicted y versus true y.

print.logisticSingle = function(var) {

    y<-var[[1]]
    x<-var[[2]]
    #Create 10 equally size folds
    folds<-cut(seq(1,length(x)),breaks=10,labels=FALSE)
    
    # A list to hold each fold's prediction
    preds<-vector()
    
    #Perform 10 fold cross validation
    for(i in 1:10) {
        #Segement the data by fold using the which() function 
        idx<-which(folds==i,arr.ind=TRUE)
        test<-x[idx]
        train<-x[-idx]
        
        # ensure y length is the same as train length for glm()
        ytrain<-y[-idx]
        
        # Fit the glm model on the training set
        glm.model<-glm(ytrain ~ train, family="binomial")
        
        # Use the fitted model on the test set
        # check whether test is composed of factors or numerics
        xgrid = 0
        if (is.factor(test)) {
            # convert to numeric levels
            xgrid = seq(levels(test))
        } else {
            xgrid = seq(min(test),max(test),by=1)
        }
        
        pred<-predict(glm.model, data.frame(train = test), type='response')
        
        # use a threshold of 0.5 to round the probability into a binary predictor - 1 (>= 0.5) or 0 (< 0.5)
        predbinary<-vector()
        for (i in 1:length(pred)) {
            if (pred[i] >= 0.5) {
                predbinary[i]<-1
            } else {
                predbinary[i]<-0
            }
        }
        
        # combine the predictions from all 10 folds
        preds<-c(preds, predbinary)  
    }
    
    # Compute the misclassification rate - use table()
    confmatrix = table(y, preds)
    
    # add false positives and false negatives together and divide by the total data length to get the 
    # missclassification rate as a proportion
    totals = 1
    if (ncol(confmatrix) == 2) {
        totals = confmatrix[1,2] + confmatrix[2,1]
    } else {
        totals = confmatrix[2,1]
    }
    
    mcr = totals/length(y)
    
    cat('Misclassification Rate: ', logisticSingle(y, x), '\n')
    print('Misclassification Table:\n')
    print(confmatrix)
}

# Test it
x<-list(birthwt$low, birthwt$age)
class(x)<-"logisticSingle"
x

### 2. Write a summary method for the findwords function of lecture 3. The
###    summary should include (at least) the total number of words and the
###    top 5 words used in the paragraph.


# Copy the text concordance function from lecture 3

firstpar = 'It was a bright cold day in April and the clocks were striking thirteen Winston Smith his chin nuzzled into his breast in an effort to escape the vile wind slipped quickly through the glass doors of Victory Mansions though not quickly enough to prevent a swirl of gritty dust from entering along with him'

findwords = function(tf) {
    # Read in the words from the text and separate into a vector
    txt = unlist(strsplit(tf,' '))
    # Create a list to store the words and their positions
    wl = list()
    class(wl) = 'wordList'
    # Loop through each word
    total = length(txt)
    for(i in 1:length(txt)) {
        # Get the current word
        wrd = txt[i]
        # Add its position to the list with the appropriate tag
        wl[[wrd]] = c(wl[[wrd]],i)
    }
    
    # Return the answer as a list
    return(wl)
}
fw<-findwords(firstpar)

# Summary method

summary.wordList = function(fw.obj) {
    print("Total No of separate Words:")
    print(length(fw))
    
    topWords<-list(name=character(0), count=integer(0))
    for (i in 1:length(fw)) {
        topWords$name[i] = names(fw[i])
        topWords$count[i] = length(fw[[i]])
    }
    topWordsOrdered<-as.data.frame(topWords)[with(topWords, order(-count)),]
    
    print("Top 5 words used are:")
    print(topWordsOrdered$name[1:5])
    return(0)
}

# Test it

summary(fw)


### 3. Write a plot method for the polyreg class created in lecture 8. The plot
###    should show the data together with all the plotted lines and a legend.

# Lecture 8 Code Start ##

lvoneout = function(y,xmat) {
    n = length(y)
    predy = vector(length=n)
    for(i in 1:n) {
        lmo = lm(y[-i] ~ xmat[-i,,drop=FALSE])
        betahat = as.vector(lmo$coef)
        predy[i] = betahat%*%c(1,xmat[i,])
    }
    return(predy)
}

polyfit = function(y,x,maxdeg) {
    n = length(y)
    Xmat = sweep(matrix(rep(x,maxdeg),nrow=n,ncol=maxdeg),2,1:maxdeg,'^')
    lmout = list()
    class(lmout) = 'polyreg'
    for(i in 1:maxdeg) {
        lmo = lm(y ~ Xmat[,1:i,drop=FALSE])
        lmo$cv.fitted.values = lvoneout(y,Xmat[,1:i,drop=FALSE])
        lmout[[i]] = lmo
    }
    lmout$x = x
    lmout$y = y
    return(lmout)
}

n = 60
x = (1:n)/n
y = rnorm(n,sin(3*pi/2*x)+x^2,sd=0.5)
maxdeg = 6
lmo = polyfit(y,x,maxdeg)
plot(x,y)

# Lecture 8 code end ###

# my plot method

plot.polyreg = function(lmo) {
    plot(lmo$x, lmo$y, xlab="X Data", ylab="Y Data")
    colours = c("blue", "red", "yellow", "green", "black", "orange")
    degrees = 6
    for (i in 1:degrees) {
        lines(lmo$x, lmo[[i]]$fitted.values, col=colours[i])
    }
    title("Polynomial Fitting to 6 degrees")
    legend("topright", legend = c("First", "Second","Third", "Fourth", "Fifth", "Sixth"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red","yellow","green","black","orange"))
}

# Test it

plot(lmo)



