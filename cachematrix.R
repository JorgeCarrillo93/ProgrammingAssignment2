## This first function, creates a special matrix
## setting and getting the value of the matrix
## setting and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## This second function, calculates the inverse of the special matrix,
## it first checks to see fi the inverse has already been calculated
## if so, gets the inverse from the cache and skips the computation
## if not, calculates the inverse and sets it in a cache via setInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
