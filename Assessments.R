makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inverse <<- solve
  getmatrix <- function() inverse
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x,...) {
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setmatrix(inverse)
  inverse
}