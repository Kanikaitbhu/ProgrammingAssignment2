## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set<-function(y){
    matrix<<-y
    inverse<<-NULL
  }
  get<-function() matrix
  setinverse<-function(solve) inverse<<-solve
  getinverse<-function() inverse
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
  
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(matrix, ...) {
  
  inverse <- matrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get()
  inverse<- solve(data, ...)
  matrix$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
