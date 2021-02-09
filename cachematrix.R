# This creates the matrix to be cached and inverted
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  sI <- function(inverse){inv <<- inverse}
  getI <- function(){inv}
  list(set, get = get, sI=sI, getI = getI)
}

# This actually does the inverting
cacheSolve <- function(x, ...){
  inv <- x$getI()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$sI(inv)
  inv
}

# my examples
prac <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
prac$get()
prac$getI()
cacheSolve(prac)
prac$getI()

prac2 <- makeCacheMatrix(matrix(16:19, nrow=2, ncol=2))
prac2$get()
prac2$getI()
cacheSolve(prac2)
prac2$getI()
