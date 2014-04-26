## This function takes matrix input of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  ## i and x can be seen similar to private objects to this class.
  ## The function itself can be seen as a constructor.
  ## Attempt to map FP to Java/CPP terms.
  i <- NULL
  set <- function(y) {
    x <<- y
    ## Every time we get a new matrix, set inverse to NULL.
    i <<- NULL
  }
  get <- function() x
  ## setInverse wil be used from within cacheSolve()
  setInverse <- function(inv) i <<- inv
  
  ## Returns the inverse of the matrix set in this class
  getInverse <- function() i
  
  ## List is created with functions set, get, setInverse and getInverse
  ## The internally defined function objects here are assigned to the list.
  ## so that they can be used outside the scope of this function.
  ## This function is basically like a constructor()
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Return a matrix that is the inverse of 'x' 
cacheSolve <- function(x, ...) {
  ## x is assumed to be a list returned by above makeCacheMatrix()
  
  ## If it has been previously computed, inv will contain valid 
  ## inverse matrix of corresponding matrix in x.
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Do below compute only once for that particular list object x.
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
