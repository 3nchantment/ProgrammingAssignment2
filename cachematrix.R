## This contains two functions that calculate and cache the inverse of a matrix. 
## After the first calculation, the stored cache is used in place of the calculation to save time.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  inverse_ <- NULL
  set <- function(y){
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse_solve) inverse_ <<- matrix_inverse_solve
  getinverse <- function() inverse_
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix unless it is already cached.
## If the matrix is already cached (inverse_ is not null) then it uses the cached value instead to save calculation time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_ <- x$getinverse()
  
  ##if the inverse was calculated previously and cached, it is used
  if(!is.null(inverse_)){ 
    message("Matrix inverse already cached, skip the inverse calculation and use the cached inverse to save time.")
    return(inverse_)
  }
  data <- x$get() ##if the inverse wasn't calculated previously and cached
  inverse_ <- solve(data) ##, it is calculated
  x$setinverse(inverse_) ##sets the value of the inverse to what is cached
  inverse_      
}
