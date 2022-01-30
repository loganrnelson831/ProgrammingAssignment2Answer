## These functions take the inverse of a matrix and store it in the cache.
## If it has to do the same computation again, it just pulls the answer 
## from the cache.
## The makeCacheMatrix function makes the matrix a special object that 
## cacheSolve can use.
## The cacheSolve then solves for the inverse of the matrix, but only does the
## computation if it hasn't been done before, otherwise it pulls it from the cache


## makeCacheMatrix creates a special matrix object which is a list of 
## functions (set, get, setinverse,getinverse) and returns it

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve looks to see if the inverse of the matrix it gets from the output
## of makeCacheMatrix has already been found, and pulls that from the cache. 
## Otherwise, it does the computation to find the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}