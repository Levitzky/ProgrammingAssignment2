## These pair of functions cache the inverse of a matrix x. 
## The cacheSolve function first checks to see if the inverse matrix has 
## already been calculated. If so, it fetches the inverse from the cache and 
## skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse matrix  
## in the cache via the setinverse function.

## Stores previously calculated inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'
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

