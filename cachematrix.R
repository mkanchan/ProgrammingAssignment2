## DESC : CACHEMATRIX.R
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly

## MAKECACHEMATRIX creates a special "matrix", 
## which is really a list containing a function to do following:
##
## Set the value of Matrix
## Get the value of Matrix
## Set the value of inverse of Matrix
## Get the value of inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverese) inv <<- inverese
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## CACHESOLVE function calculates inverse of the special "matrix" 
## created with the above function, it does following:
##
## It first checks if inverse of matrix is already calculated
## If yes, it gets the inverse from cache and skip rest of the computation
## Otherwise, it computes the inverse of the matrix and sets the value
## of inverse in cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## function assumes that matrix x is invertible
        
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data..")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx,...)
        x$setinverse(inv)
        inv
}
