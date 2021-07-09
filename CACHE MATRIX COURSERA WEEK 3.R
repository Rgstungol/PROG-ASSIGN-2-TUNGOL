makeCacheMatrix <- function( M = matrix()){
  inv <- NULL
  set <- function(N){
    M <<- N
    inv <<- NULL
  }
  get <- function() {M}
  setInverse <- function(inverse) (inv <<- inverse)
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cachesolve <- function (M, ...){
  inv <- M$getInverse()
  if(!is.null(inv)){
    message("getting data")
    return(inv)
  }
  mat <- M$get()
  inv <- solve(mat, ...)
  M$setInvert(inv)
  inv
}

