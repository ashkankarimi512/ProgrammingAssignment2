##
## Programming assignment 2: Lexical Scoping
##
##
## function: makeCacheMatrix
## This function returns a list of function used for caching a matrix
## and its inverse. 
## The returned list contains: set, get, setinvMat, getInvMat
##
## Usage:
##  a <- matrix( c(30, 72, 87, 96, 74, 46, 86, 85, 57), nrow=3, ncol=3)
##  ca <- makeCacheMatrix( a )
##  ainv <- cacheSolve( ca )
##  ca$get()
##  ca$getInvMat()
##
## Test that the inverse is correct:
##       ainv %*% a        must equal the Identity matrix.
##
## Notes:  Operator <<- used to assign values to a variable with in the
## specific environment.  Here, the function caches the supplied matrix
## the inverse within special caced matrix returned.  The list returned 
## contains for method to get/set the matrix and its inverse.
## 

makeCacheMatrix <- function(xmat = matrix()) {
        matInv <- NULL
        set <- function(y) {
                xmat <<- y
                matInv <<- NULL
        }
        
        get <- function() xmat
        
        setInvMat <- function(y) matInv <<- y
        
        getInvMat <- function() matInv
        
        list(set = set, get = get, setInvMat = setInvMat, 
             getInvMat = getInvMat)
}


## function: cacheSolve
##
## This function calculates the inverse of the xmat and caches the
## matrix and the inverse of the matrix. The inverse is computed using
## solve function from the R base package.
##
## The function returns the cached Inverse matrix.
## 
##
cacheSolve <- function(xmat, ...) {
        ## Return a matrix that is the inverse of 'xmat'
        
        ## get the cached inverse matrix
        matInv <- xmat$getInvMat()
        
        ## Test if the cached inverse matrix is not null return it.
        if ( !is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        
        ## get the cached matrix 
        data <- xmat$get()
        
        ## Calculate the inverse and save it in the
        ## cache.  Assume the martrix has an inverse.
        ## To be done for future enhancement : 
        ## check if the matrix has an inverse!
        
        matInv <- solve(data)
        xmat$setInvMat( matInv )  ## Save the inverse to the cache
        matInv
}
