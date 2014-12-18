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

makeCacheMatrix <- function(xmat = matrix()) {
        matInv <- NULL
        set <- function(y) {
                xmat <<- y
                matInv <<- NULL
        }
        
        get <- function() xmat
        
        setInvMat <- function(y) matInv <<- y
        
        getInvMat <- function() matInv
        
        list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


## function: cacheSolve
##
## This function calculates the invese of the xmat or returns the cached Inverse
## it it has not been changed.
#
cacheSolve <- function(xmat, ...) {
        ## Return a matrix that is the inverse of 'xmat'
        matInv <- xmat$getInvMat()
        if ( !is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        
        data <- xmat$get()
        ## Assume the martrix has an inverse.
        ## TBD : check if the matrix has an inverst!
        
        matInv <- solve(data)
        xmat$setInvMat( matInv )
        matInv
}
