## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
##retrieve the inverse from the cache.

matrix_nat <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix_nat$get()
cacheSolve(matrix_nat)     ##inverse returned after computation
cacheSolve(matrix_nat) #inverse returned from cache
matrix_nat$getinverse()
