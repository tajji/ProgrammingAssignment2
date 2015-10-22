## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a "list" which contains functions to get and set the
## value of the matrix it encloses. Additionally, there are functions to get and set the  
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the special matrix created with the above
## function. It first checks to see if the inverse has already been calculated and cached. If
## so, it gets the cached value. Else, it calculates the inverse and returns it after caching
## it.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
