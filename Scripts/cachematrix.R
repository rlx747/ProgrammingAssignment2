## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function

## the function makeCacheMatrix makes an object(similar to a Java or Kotlin object)  which contains a variable(matrixInverse) to hold the inverse
##  of the inputted matrix
## it also holds a list of 4 functions to set and get the input matrix variable(which is x) and to set and get the inverse of that matrix(setInverse,getInverse)
## Use Case:
##          a <- makeCacheMatrix(matrix(1:4, 2,2)) 
##             now a is an object which holds a 2by2 matrix  BUT it doesn't yet hold value of inverse

##           OR use set function
##          matrix_1 <- matrix(1:4,2,2)
##          a <- makeCacheMatrix() 
##              now a is an object which holds a 1by1 matrix with value NA(essentially just a placeholder) AND it doesn't yet hold value of inverse
##          a$set(matrix_1)
##              now we have set the input matrix AND it doesn't yet hold value of inverse

##          cacheSolve(a)  calculates the inverse and places it in the appropriate variable inside a(matrixInverse),  and then it returns that inverse
##          To verify that it is the inverse simply multiply the original matrix by the inverse and should result in identity matrix
##          Example:   a$get() %*% cacheSolve(a)      this is matrix multiplication
##                      result is a 2by2 identity matrix 

makeCacheMatrix <- function(x = matrix()) {
        
    ##  the <<- operator inside the setInverse function causes R to search for the existence of a variable called matrixInverse in the parent environments;
     ##   since it finds a variable called matrixInverse in the outer function, R then assigns the value of inverse to that same variable
    ##                                                          so that it is available through the makeCacheMatrix object. 
    ##  Note: this function DOES NOT calculate the inverse of a matrix! It simply makes an object that contains a variable to hold the inverse after it is set with
    ##  setInverse()
    ## Use cacheSolve function to generate the inverse
    
    ##  Similarly, the <<- operator inside the set function assigns the input matrix y to the x defined in the arg(which is visible inside the entire scope
    ##    of the makeCacheMatrix);
    ##  also when calling the set function the value of the outer variable matrixInverse is reassigned to NULL
    ##     (because otherwise you would have a new value for the input matrix and the old value for the inverse would still be present in matrixInverse)
    
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Write a short comment describing this function
##  purpose is to potentially save time on time-consuming inverse calculations on a large matrix by saving the inverse to a variable for future use

##  this function takes an object of type makeCacheMatrix-- example: a <- makeCacheMatrix(matrix(1:4,2,2))
## and returns the inverse of the matrix --  example: cacheSolve(a); this returns the inverse
## since the function first checks to determine if the input object already has a value for inverse,
##  it can save time by simply returning that value instead of recalculating the inverse again; if there is no previous value, then it is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' where 'x' is and object of makeCacheMatrix
    
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}

##  From ?"<<-"
## The operators <<- and ->> are normally only used in functions,
##  and cause a search to be made through parent environments for an existing definition of the variable being assigned. 
##  If such a variable is found (and its binding is not locked) then its value is redefined, otherwise assignment takes place in the global environment.
