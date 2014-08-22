##These two functions paired together will calculate the inverse of a matrix. The first time cacheSolve is called, it
##will run the calculation and store the inverse matrix. The second time it is called, it will display the cached matrix
##without recalculating.

##This function takes a matrix and creates a template that is compatible with the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will test to see if the matrix has been inverted and stored.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("This solution is already stored, retrieving data from cache...")
    return(m) #This will display the solution instead of recalculating.
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
