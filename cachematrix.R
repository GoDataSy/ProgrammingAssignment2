## makeCacheMatrix and cacheSolve can be used together to compute a square invertible matrix's inverse matrix, or 
## to retrieve the inverse matrix from a "cache" if it has been previously computed. makeCacheMatrix provides functions 
## for getting and setting the values of: x, a current square invertible matrix; inv, an inverse matrix; and oldx, a 
## previously stored square invertible matrix. cacheSolve takes a makeCacheMatrix object as an argument and computes 
## the inverse of the object's current matrix, or retrieves the inverse matrix from the "cache" if it has been previously
## computed.  
##
## Since makeCacheMatrix uses (1) non-local assignment of x, inv and oldx (via the <<- operator), and (2) the function
## returns a list() of its own functions, a makeCacheMatrix object's functions share a common environment,
## and those functions can be called to update x, inv and oldx in that shared environment. 
## 
## To use the functions:
##
## First, create a makeCacheMatrix object, passing it a square invertible matrix 
## > myMatrix <- makeCacheMatrix(matrix(c(4,3,3,2),2,2)). 
##
## Alternatively, create the makeCacheMatrix object, passing it nothing; then call the setx() function of the 
## object, passing it the square invertible matrix
## > myMatrix <- makeCacheMatrix()
## > myMatrix$setx(matrix(c(4,3,3,2),2,2))
## 
## Then, to get the inverse of the matrix, pass the object to the cacheSolve() function 
## > cacheSolve(myMatrix)
##       [,1]  [,2]
## [1,]    -2    3
## [2,]     3   -4
##
## To retrieve the stored inverse matrix, call cacheSolve(myMatrix) again.
## > cacheSolve(myMatrix)
## getting cached inverse matrix
##       [,1]  [,2]
## [1,]    -2    3
## [2,]     3   -4
##
## Reference: Software for Data Analysis: Programming with R by JM Chambers, pgs 126-128


## makeCacheMatrix takes an optional square invertible matrix as an argument, and specifies functions that can be 
## called to get and set the values of x, inv and oldx.

makeCacheMatrix <- function(x = matrix()) {

        oldx <- NULL
        inv <- NULL
        
        getx <- function() x
        
        setx <- function(matrx) {
                x <<- matrx
        }

        getinv <- function() inv
        
        setinv <- function(inverse){ 
                inv <<- inverse
        }
       
        
        getoldx <- function() oldx                
                
        setoldx <- function(x){
                oldx <<- x                
        }
        
        
        ## list of callable functions
        list(getx = getx, setx=setx, 
             getinv = getinv, setinv = setinv,
             getoldx = getoldx, setoldx = setoldx)
        
}


## cacheSove takes a makeCacheMatrix object as an argument, and tries to retrieve the value of the object's 
## inverse matrix from a "cache". If the value of the inverse matrix is NULL, or if the object's matrix is new, 
## cacheSolve computes the inverse matrix and return it.

cacheSolve <- function(x, ...) {  ## x is a makeCacheMatrix object
        inv <- x$getinv()
        data <- x$getx()
        olddata <- x$getoldx()
        
        if(!is.null(inv) && identical(data, olddata)) {
                message("getting cached inverse matrix")
                return(inv)
        }

        inv <- solve(data, ...)
        x$setinv(inv)
        x$setoldx(data)
        inv
}
