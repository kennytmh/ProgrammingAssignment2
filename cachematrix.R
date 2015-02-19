## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Most matrix inversion are a costly computation and it is logically better
# to cache the inverse of a matrix rather than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1st - To set the value of the matrix
# 2nd - To get the value of the matrix
# 3rd - To set the value of inverse of the matrix
# 4th - To get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If it has, it gets the result and skips the
# computation. If not, it computes the inverse.
# Following which, it will setsthe value in the cache via setinverse function.

# An assumption made is that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}