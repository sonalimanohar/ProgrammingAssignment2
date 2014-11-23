makeCacheMatrix <- function(xsolve = matrix()) 
{ ## makeCacheMatrix is a Function which does the following:
  ## -- Creates a matrix via the "set" function.
  ## -- Creates 'attributes' such as set, get, setsolve, getsolve and assigns it to the matrix
  ## -- Resultant matrix is now capable of performing tasks such as setting some values and also getting them
  ## -- Logic for inversing the matrix is not included in this function. It is a part of the calling function cacheSolve(msolve()).
  ## -- Step #1 is to initialize this function and assign it to a matrix. For eg: cmat <- makeCacheMatrix()
  
  inv <- NULL                # inv is a vector that is shared by both functions. Its value is set in the setsolve() function
  set <- function(y)         # Call the set() function as step #2 and pass an actual matrix as parameter. cmat$set(matrix(1:4,2,2))
    {                        # set() is a "Closure" function. It can access variables from its parent function- makeCacheMatrix
      xsolve <<- y           # setting the value of parameter from parent function. Hence <<- operator.
      inv <<- NULL
    }
  get <- function() xsolve   # Step3: Calling cmat$get() should display the matrix provided in step #2. 
  setsolve <- function(solve) inv <<- solve   
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


 cacheSolve <- function(msolve=matrix(), ...) 
{
  ## cacheSolve is called as step #4 and is passed the matrix created in step #1. For eg: cacheSolve(cmat)
  inv <- msolve$getsolve()    ## Value of inv is checked to determine if empty or not. "not empty" = cached.
  if(!is.null(inv))           ## If "not empty" then return cached data and exit the function cacheSolve.
    {
      message("getting cached data")
      return(inv)
    }
  
  ## Continue with the following lines if value of inv is NULL. 
  ## inv will be NULL when cacheSolve is executed for the first time. 
  ## for all subsquent runs, the control will come to this point.
  
  data <- msolve$get()       ## this statement will store the matrix returned by get() into data
  inv <- solve(data, ...)    ## Now THIS is where the actual inversion of the matrix takes place. solve(<matrix>) is used to inverse a matrix.
  msolve$setsolve(inv)       ## setsolve is set to the value of inv. 
  inv                        ## inversed matrix is returned to the console.
}
