### Caching the Inverse of a Matrix
### Coursera R Programming Assignment, class rprog-004, June 2014
### Andreas Pawlik

### Matrix inversion is usually a costly computation and there may be some
### benefit to caching the inverse of a matrix rather than computing it
### repeatedly. This files contains a pair of functions that cache the 
### inverse of a matrix.

## Example R session:
##
## > source("cachematrix.R")
##
## > M <- matrix(c(3,1,2,4),2,2)    ## matrix M to be inverted
##
## > x <- makeCacheMatrix(M)        ## helper matrix
##
## > Minv <- cacheSolve(x)          ## compute the inverse
##
## > Minv                           ## display the inverse
##     [,1] [,2]
##[1,]  0.4 -0.2
##[2,] -0.1  0.3
##
## > M %*% Minv                     ## verify that inverse is correct 
##     [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## > Minv <- cacheSolve(x)          ## get inverse from cache
## getting cached data
## 
## > Minv                           ## display the inverse
##      [,1] [,2]
## [1,]  0.4 -0.2
## [2,] -0.1  0.3


## This creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		
            xinv <- NULL
            set <- function(y) {
                    x <<- y
                    mxinv <<- NULL
            }
            get <- function() x
            setinv <- function(solve) xinv <<- solve
            getinv <- function() xinv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
    

}


## This function returns the inverse of the chached matrix x. If the
## inverse has already been computed and cached in a previous call to
## this function, and if the matrix x has not changed, then this 
## function returns the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


            xinv <- x$getinv()
            if(!is.null(xinv)) {
                    message("getting cached data")
                    return(xinv)
            }
            data <- x$get()
            xinv <- solve(data, ...)
            x$setinv(xinv)
            xinv


}
