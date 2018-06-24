# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function


# The purpose of these two functions are to store the inverted matrix when solved 
#and not solving again if it is already solved 

# The 'makeCacheMatrix' function creates a special "matrix" 
# containg four nested functions: 'set', 'get', 'setsolve' & 'getsolve'. 
#   - 'set' is used to assign a matrix to the variable 
#   - 'get' is used to recall the existing matrix 
#   - 'setsolve' is used to assign an inverted matrix to the variable
#   - 'getsolve' is used to recall the stored inverted matrix.

# This function returns a list of values from the nested funcitons 
# that will be used by 'cacheSolve' to get or set the inverted matrix in cache.




makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# Write a short comment describing this function


# this function will check to see if the inverse of the matrix is stored in cache.
# If the inverted matrix does not exist then the inverse is calculated using solve



cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
