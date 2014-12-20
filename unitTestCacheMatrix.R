##
## Unit Test for : cacheMatrix.R
##

unitTest_CacheMatrix <- function() {
        a <- matrix( c(30, 72, 87, 96, 74, 46, 86, 85, 57), nrow=3, ncol=3)
        print(a)
        
        ca <- makeCacheMatrix( a )
        ainv <- cacheSolve( ca )
        print(ainv)
        
        ainv <- cacheSolve( ca )
        
        print("Matrix A")
        print(ca$get() )
        
        print("A inverse")
        print(ca$getInvMat() )
        
        print("A inverse * A")
        print(ainv %*% a )
        ca
}