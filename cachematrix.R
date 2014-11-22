makeCacheMatrix <- function(xsolve = matrix()) 
{ ## Function which does the following:
  ## -- Creates a matrix via the "set" function.

  inv <- NULL
  set <- function(y) 
    {
      xsolve <<- y
      inv <<- NULL
    }
  get <- function() xsolve
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


 cacheSolve <- function(xsolve=matrix(), ...) {
  inv <- xsolve$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- xsolve$get()
  inv <- solve(data, ...)
  xsolve$setsolve(inv)
  inv
}
