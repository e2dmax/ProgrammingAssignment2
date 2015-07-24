## The following functions work together to make it faster to retreive
## the solve reuslt of a matrix if the solution is needed more than once.
## Pass a matrix to the makeCacheMatrix and it returns a special matrix
## that can then be used with the cacheSolve function.


## makeCacheMatrix takes a matrix as input. It stores the input matrix 
## within its scope and returns a list of functions for retrieving and 
## manipulating the matrix and its cached result.
## $set is used to set or reset the matrix
## $get is used to retreive the matrix
## $setsolve is used to store(cache) the solved matrix
## $getsolve is used to retreive the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve takes the special matrix as input and first checks to see
## if the matrix solution already exists.If it does then the cached result 
## is retreived and returned.  Else if the solution does not already exist
## then the matrix is solved and stored in the special matrix and then
## returns the result.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
