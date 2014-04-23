## Below are two functions that are used to create
## a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) i <<- inverse
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix" 
## created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x' 
   ## First check if the inverse of x is already stored in cache
   i <- x$getInverse()
   if(!is.null(i)) {
      ## If so, just return the cached value of the inverse of 'x'
      message("getting cached data")
      return(i)
   }
   ##If there is no value cached, use the solve function to compute the
   ##Inverse of the matrix 
   data <- x$get()
   i <- solve(data, ...)
   ##Save the result in cache and return the inverse of 'x'
   x$setInverse(i)
   i
}
