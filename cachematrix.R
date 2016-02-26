## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix which can store a matrix and an inverse of a matrix
#  input: a matrix  
# 

makeCacheMatrix <- function(x = matrix()) {
  
  MatInverse <- NULL
  
  set <- function(m)
    {
      x <<- m
      MatInverse <<- NULL
    }
  
  get <- function()
    {
      x
    }
  
  setInverse <- function(solve)
  {
    MatInverse <<- solve
  }
    
  
  getInverse <- function()
  {
    MatInverse
  }
    
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This fuction calculates the inverse of a given matrix and stores the value into cache.
# before calculating it will check the availability in the cache, if available it will fetche 
# from the cache if not it will calculate the inverse .

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MatInverse <- x$getInverse()
  
  #Checking for availability in the cache
  if(!is.null(MatInverse)) {
    message("getting cached data")
    return(MatInverse)
  }
  #calculating the inverse of a matrix
  data <- x$get()
  MatInverse <- solve(data, ...)
  x$setInverse(MatInverse)
  MatInverse
}
