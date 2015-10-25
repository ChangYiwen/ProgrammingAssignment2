## The two functions, makeCacheMatrix and CacheSolve, work together 
## for calculating the inverse of a matrix, and store the calculating
## result as cache.

## The function, makeCacheMatrix, generates matrix, and cache the
## calculation result of inverse matrix, which done by cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
          Inv <- NULL
          set <- function(y) {
                    x <<- y
                    Inv <<- NULL
           }
           get <- function() x
           setInv <- function(solve) Inv <<- solve
           getInv <- function() Inv
           list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}


## The function, cacheSolve, calculates the inverse of the matrix, 
## which created by makeCacheMatrix, for its first time computation. In 
## case the same matrix had been calculated, cacheSolve will
## get the cache, which done by makeCacheMatrix after the first time
## matrix inverse calculation, directly. 
cacheSolve <- function(x, ...) {
          Inv <- x$getInv()
          if(!is.null(Inv)) {
                   message("getting cached data")
                   return(Inv)
          }
         data <- x$get()
         Inv <- solve(data, ...)
         x$setInv(Inv)
         Inv
}
