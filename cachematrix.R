## In these functions, caching of inverse of a matrix is performed. First, the inverse is initialised. A list is created with the inverse and the matrix. Following this, in the second function, which is the caching function, it is first checked if the inverse is already cached. If it is, the cached value is returned. Else inverse is calculated.

## makeCacheMatrix function returns a list which can be accessed to get the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmat <- function(n=matrix()) {
    x <<- n
    i <<- NULL
  }
  getmat <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(setmat = setmat, getmat = getmat, setinverse = setinverse, getinverse=getinverse)
}

## cacheSolve function returns the inverse of the matrix. If value has been cached before, it uses the cached value. Else, it calculates it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("\nGetting Cached Data...")
    return(i)
  }
  data <- x$getmat()
  message("\nCalculating Inverse of Matrix...")
  i <- solve(data, ...)
  x$setinverse(i)
  message("\nInverse Matrix:")
  i
}
