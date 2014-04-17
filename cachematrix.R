## The functions makeCacheMatrix(x) and cacheSolve(x) provides
## the functionality to calculate and cache the result of a inverse operation 
## of a Matrix x
## 
## Usage example:
##
## myMatrix <- matrix(c(1,2,3,4), 2, 2) # Creates a matrix
## myCacheableMatrix <- makeCacheMatrix(myMatrix) # Creates a cacheable representation of myMatrix
## myInverse1 <- cacheSolve(myCacheableMatrix)  # Calculates and returns inverse of myMatrix and cache result
## myInverse2 <- cacheSolve(myCacheableMatrix)  # Returns inverse of myMatrix from cache
##
## In the usage example above myInverse1 was calculated, but myInverse1 was returned
## from the cache.


## The function, makeCacheMatrix(x), is used to create a cacheable matrix
## as shown in the example below
## 
## Usage example:
##
## myMatrix <- matrix(c(1,2,3,4), 2, 2) # Create a matrix
## myCacheableMatrix <- makeCacheMatrix(myMatrix) # Creates a cacheable representation of myMatrix
##
## The result from makeCacheMatrix is to be used as input for
## the function, cacheSolve, shown below.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function, cacheSolve(x), calculates and caches the inverse of
## a Matrix, x,  created via the function, makeCacheMatrix, shown above. 
##
## The inverse of x is only calculated the first time cacheSolve(x) is called. 
## For all succeeding calls to cacheSolve(x) the inverse is returned from cache. 
##
## Usage example: (See example at beginning of the file)
##
## If x changes then the inverse of x will be recalculated and recached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
