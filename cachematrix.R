#Well i used the example and changed it with the solve function.

#This function creates a matrix object than can cache its inverse
makeCacheMatrix <- function(x = matrix(sample(1:10,16),4,4)) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #lets define the functions needed to solve
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv 
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

#we compute the inverse of the matrix object  given by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  
  if(!is.null(inv)){
    # we return the cached matrix
    message("obtener datos cacheados")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setsolve(inv)
  inv ##return inverted matrix
}