## These two functions cache the inverse of a matrix, saving computation time
## by retrieving a cached value if it has already been calculated

## The function makeCacheMatrix creates an object storing the original matrix value
## and what will be the cached value (the inverse).  There are four functions included
## which get the inverse value and set the inverse value

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(matrix) {
          x <<- matrix
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The cacheSolve function accesses the object function created when makeCacheMatrix was called
## If the inverse has not yet been calculated, this function calculates it, stores it, and returns it.
## IF the inverse has already been calculated, this function simply fetches it, saving computation time.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
