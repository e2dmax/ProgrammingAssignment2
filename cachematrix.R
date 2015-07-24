## The following functions work together to make it faster to retreive
## the inverse of a matrix if the solution is needed more than once.
## Pass a matrix to the makeCacheMatrix and it returns a object
## that can then be used with the cacheSolve function.


## makeCacheMatrix takes a matrix as input. It stores the input matrix 
## within its scope and returns a list of functions for retrieving and 
## manipulating the matrix and its cached inverse.
## $set is used to set or reset the matrix
## $get is used to retreive the matrix
## $setsolve is used to store(cache) the inverse matrix
## $getsolve is used to retreive the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the object from makeCacheMatrix as input and first 
## checks to see if the inverse of the matrix exists. If it does then 
## the cached inverse is retreived and returned. Else if the inverse 
## does not already exist then the matrix is solved and the inverse is 
## stored in the object. It then returns the inverse.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
