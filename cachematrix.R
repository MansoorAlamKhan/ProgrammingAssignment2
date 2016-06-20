## Put comments here that give an overall description of what your
## functions do

## This function makes a special matrix which can cache the inverse of a matrix. It also adds the functions of set, get, setinverse and getinverse functions for the object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()  x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the inverse of the matrix from the cache if it exists, otherwise it calculates the cache using the cacheSolve function, sets it and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
