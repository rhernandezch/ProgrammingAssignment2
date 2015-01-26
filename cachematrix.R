makeCacheMatrix <- function(x = matrix()) {
  # Function that returns a matrix object containing four functions required to
  # sucesfully store and cache the inverse of the matrix and the variable name
  # that was passed as an argument to makeCacheMatrix.
  # 
  # This matrix object or 'matrix wrapper' must be built before caching the
  # inverse. If the matrix is changed it is not necessary to build makeCacheMatrix
  # again, this can be done using the set() method or function, e.g., 
  # if matrix.object <- makeCacheMatrix(x), then use matrix.object$set(new.matrix) 
  #
  # Args:
  #   x: A n by n matrix that it'll be assumed to be invertible.
  #
  # Returns: 
  #   Matrix object which is list of four functions: set, get, setinv, getinv
  inv <- NULL
  set <- function(y) {  # set should be used to flush the cache if x changed
    x <<- y
    inv <<- NULL
  }
  get <- function() x # stores a copy of the matrix x in the object
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
    
  list(set = set, get = get, setinv = setinv, getinv = getinv, 
       var.name = var.name)
}

cacheSolve <- function(x.obj, ...) {
  # Function that takes the matrix object created by makeCacheMatrix as an
  # argument and checks if a cached version of the inverse matrix exists. If so,
  # it returns such matrix; otherwise, it calculates and stores the inverse.
  #
  # Args:
  #   x.obj: The object returned by calling makeCacheMatrix(x)
  #
  # Returns: 
  #   The inverse of the matrix x
  
  inv <- x.obj$getinv() # searches for a cached version of the inverse
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x.obj$get()
  inv <- solve(data) # calculates the inverse
  x.obj$setinv(inv) # sets the inverse via the setinv() function
  inv
}
