## The following functions will take a square matrix and figure out its inverse.  
## Then the square matrix and the inverse are cached
## Once a matrix is cached, the cached inverse of the matrix is what is returned
## Ex:
##         m <- matrix(c(-1, -2, 1, 1), 2,2)
##         x <- makeCacheMatrix(m)
##         x$get()
##         [,1] [,2]
##         [1,]   -1    1
##         [2,]   -2    1
##         
##         inv <- cacheSolve(x)
##         inv
##         [,1] [,2]
##         [1,]    1   -1
##         [2,]    2   -1
##         
##         > inv <- cacheSolve(x)
##         getting cached data
##         > inv
##         [,1] [,2]
##         [1,]    1   -1
##         [2,]    2   -1




## Creates four functions that set, get, setInv and getInv to set and get
## both the square matrix and the inverse

## 'matrix' is a square matrix that is to be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(z) inv <<- z
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Gets the cached square metrix and the sets and returns the inverse of that matrix using the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
