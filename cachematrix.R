## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { 
  inv <- NULL
  set <- function(y){
         x << - y
         inv <<- NULL 
  }
  get <- function() {x}
  setInvers <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        ## Return a matrix that is the inverse of 'x'
}

cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,..)
  x$setInvers(inv)
  inv
}