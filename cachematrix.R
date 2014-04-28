## This file contains functions that manipulates matrix and its
## inverse and enables caching the inverse to avoid recomputation

## This function take a matrix as input and creates a list with
## function variables that enables getting/setting the matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializes inverse of the CacheMatrix
  i <- NULL
  
  ## Setter for the CacheMatrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Getter for the CacheMatrix
  get <- function() x
  
  ## Setter for the inverse of the CacheMatrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Getter for the inverse of the CacheMatrix
  getinverse <- function() i
  
  ## Creates list that is returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a list created with the makeCacheMatrix
## function above and verifies if the inverse of the matrix used
## as the input of the makeCacheMatrix function was already computated:
## if positive, it returns the inverse already calculated saved in cache,
## otherwise, it calculates the inverse and saves it in cache

cacheSolve <- function(x, ...) {
  ## Gets inverse of the CacheMatrix
  i <- x$getinverse()
  
  ## Verifies if the inverse exists, if so, returns its cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If inverse of CacheMatrix does not exists:
  ## Gets CacheMatrix
  data <- x$get()
  
  ## Calculates inverse of CacheMatrix
  i <- solve(data, ...)
  
  ## Stores inverse of CacheMatrix in cache
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
