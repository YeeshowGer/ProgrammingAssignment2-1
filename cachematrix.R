## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                x <<- y
                i <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) i <<- inverse
            getInverse <- function() i
            list(set = set,
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
    }


#### cacheSolve is a function that compute the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            i <- x$getInverse()
            if (!is.null(i)) {
                message("getting cached data")
                return(i)
            }
            mat <- x$get()
            i <- solve(mat, ...)
            x$setInverse(i)
            i
    }


