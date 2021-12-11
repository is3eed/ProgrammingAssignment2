## Put comments here that give an overall description of what your
## first method is makeCacheMatrix
## second methods is casheSolve 

## Write a short comment describing this function

makeCacheMatrix<- function(myMatrix= matrix()) {
  inverse1 <- NULL
  set <- function(new) {
    myMatrix <<- new
    inverse1 <<- NULL
  }
  get <- function() myMatrix
  setInverse <- function(inverse) inverse1 <<- inverse
  getInverse <- function() inverse1
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(myMatrix, ...) {
  inverse1 <- myMatrix$getInverse()# matrix that is the inverse of 'myMatrix'
  if (!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
  }
  mat <- myMatrix$get()
  inverse1 <- solve(mat, ...)
  myMatrix$setInverse(inverse1)
  inverse1
}
