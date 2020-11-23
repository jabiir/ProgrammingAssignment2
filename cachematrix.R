# Matrix inverversion is usually a costly computation and some benefit may be by cashing the inverse of a matrix rather compute it again and again.

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



# the following function returns the inverse of the matrix. when inverse function is alrady computed take the results and skip computation.

cacheSolve <- function(x, ...) { 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve data
  x$setinverse(inv)
  inv
}

##sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,] 1.00 -0.25
## [2,] -0.25 1.00
## >