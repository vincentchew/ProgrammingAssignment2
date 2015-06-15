# The following functions calculate the inverse of a matrix and caches it
# for future use. This cached value will negate the need for
# recalculation of the inverse matrix given that the value of the matrix remains static


# This function creates a special "matrix" object that can cache its inverse and
# executes the following tasks:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # Assign cache i
  i <- NULL
  
  # Sets input argument of the parent function to input argument of child function
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  # Returns the value of the matrix from the parent function(x)
  get <- function() x
  
  # Assigns i to the calculated value of the inverse matrix
  set_inverse <- function(inverse) i <<- inverse
  
  # Retruns the value of the inverse matrix
  get_inverse <- function() i
  
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Check if inverse calculation has been done and cache in i, return i if yes
  i <- x$get_inverse()
    if(!is.null(i)){
      message('getting cache data')
      return(i)
    }
  
  # Calculate inverse of matrix if i is NULL
  data <- x$get()
  i <- solve(data)
  x$set_inverse(i)
  i
  
}
