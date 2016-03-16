# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inverseMat) inverse<<- inverseMat
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
}

## Sample run:
## a <- makeCacheMatrix(matrix(1:4, byrow=T, nrow=2))
##a$get()
##	 [,1] [,2]
##[1,]    1    2
##[2,]    3    4
## No cache in the first run
##cacheSolve(a)
##	 [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
## Retrieving from the cache in the second run
## > cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5

