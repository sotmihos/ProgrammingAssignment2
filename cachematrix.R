## These two functions created and access a "special" matrix. This "special"
## matrix is actually a list of functions along with some data that are carried
## along through lexical scoping. The data is a matrix variable defined in the
## first function.

## This function creates the "special" matrix
##
## Args:
##   x: the actual matrix
##
## Returns:
##   A list of functions for setting(set) and getting(get) the matrix value and
##   for setting(set) or getting(get) its inverse value. The matrix is stored
##   in the defining environment of these functions, and is acces through
##   lexical scoping

makeCacheMatrix <- function(x = matrix()) {
     inv = NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
}


## This function returns the inverse of a "special" matrix. It checkes whether
## the inverse is cached in the matrix object. If it is, it returns its value.
## Otherwise, it computes, caches and returns its value.
##
## Args:
##   x: The special matrix. A list created using the makeCacheMatrix function.
##
## Returns:
##   A numeric equal to the inverse of the "special" matrix.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if (!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setInverse(inv)
     inv
}