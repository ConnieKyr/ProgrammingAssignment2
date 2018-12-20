## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## The following functions can cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of the inverse matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
            inv <- NULL
            set <- function(y) {
                x <<- y
                inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inversed) inv <<- inversed
            getinverse <- function() inv
            list(set = set, get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)

}


## The following function checks if there's already a cached version of the inverse of the special "matrix", 
## and retrieves it, otherwise, it calculates the inverse of the special "matrix" returned by makeCacheMatrix above and 
## caches it for next time. Overall we assume that the matrix supplied is always invertible. 

cacheSolve <- function(x, ...) {

  
              inv <- x$getinverse()
              if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)
              x$setinverse(inv)
              inv
  
        
}
