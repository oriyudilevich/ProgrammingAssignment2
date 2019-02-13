## The functions below are used to compute the inverse 
## of a square matrix. The computed inverse is saved to the cache
## and is retrieved from the cache when called again. 
##
## How to use the functions:
##    CacheMatrix <- makeCacheMatrix()          # Initialize a CacheMatrix object
##    CacheMatrix$set(matrix(c(1,1,1,0),2,2))   # store a matrix in the cache
##    CacheMatrix$get()                         # retrieve the stored matrix from the cache
##    cacheSolve(CacheMatrix)                   # get the inverse of the matrix
##                                              # in this first call, the inverse is computed and store in the cache
##    cacheSolve(CacheMatrix)                   # in this second call, the inverse is retrieved from the cache
##    CacheMatrix$set(matrix(c(1,1,1,0),2,2))   # store a different matrix in the cache
##    cacheSolve(CacheMatrix)                   # get the inverse of the matrix
##                                              # the inverse will be computed and store in the cache



## makeCacheMatrix returns a CacheMatrix object that is used
## to store a matrix and its inverse in the cache. As an optional argument,
## you can pass the matrix to be stored in the cache.

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


## cacheSolve takes a CacheMatrix object as an argument. It returns the 
## inverse of the matrix stored in CacheMatrix from the cache. If the inverse
## has not been computed yet, it will compute it and store it in the cache. 

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("inverse retrieved from cache")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      message("inverse computed")
      inv
}
