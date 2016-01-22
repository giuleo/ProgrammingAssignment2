## The functions take in a numeric matrix and compute its inverse
## caching the value of the matrix and that of the inverse.


## makeCacheMatrix takes in a numeric matrix and creates a list 
## of functions to set/get the matrix itself or its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if the matrix entered is numeric and exit otherwise
  if(!is.numeric(x)) {
    stop("the matrix entered is not of numeric type")
  }
  
  # Initialize the value for the inverse variable
  inv <- NULL
  
  # Function set - it changes the value of the matrix to the new entered
  # value y and initializes the value for inv
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # Function get - it just returns the value of the entered matrix
  get <- function() x
  
  # Function setinverse - it assigns to the inv variable the value 
  # of inverse, computed in the function cacheSolve
  setinverse <- function(inverse) inv <<- inverse
  
  # Function getinverse - it picks and returns the value of inv
  getinverse <- function() inv
  
  # Product of the makeCacheMatrix - a list collecting
  # the four functions defined therein
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The next function returns a matrix that is the inverse of 'x'.
## It looks into the makeCacheMatrix function and returns the
## value cached there or computes the solution if cache is empty.

cacheSolve <- function(x, ...) {
  
  ## The function gets the value of the inverse of the matrix 
  ## from makeCacheMatrix if it was already computed
  
  inv <- x$getinverse()
  
  ## ... and print it out
  
  if(!is.null(inv)) {
    message("...getting cached data...")
    return(inv)
  }
  
  ## If it wasn't not already computed, it gets the data
  ## from makeCacheMatrix, ...
  
  data <- x$get()
  
  ## ... computes the inverse of the matrix, ...
  
  inv <- solve(data, ...)
  
  ## ... sets the computed value in makeCacheMatrix, ...
  
  x$setinverse(inv)
  
  ## ... and returns this value out.
  
  inv
}
