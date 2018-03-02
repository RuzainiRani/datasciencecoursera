makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_ivn <- function(inverse) inv <<- inverse
  get_ivn <- function() inv
  list(set=set, get=get, set_inv=set_ivn, get_ivn=get_ivn)
}
cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_ivn(inv)
  inv
}
