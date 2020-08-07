## The functions use the cache mechansism to better the perfromance
## functions do

## This funtion is used to cache the matrix information

makeCacheMatrix <- function(x = matrix()) {
  
}


## This is used to resolve the cache functionality

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}
##on the  lines of the illustrations provided
##The functions use the cache mechansism to better the perfromance
##This function is used to cache the matrix information


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## This is used to resolve the cache functionality


cacheSolve <- function(x, ...) {
  
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}