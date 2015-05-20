## Caching the Inverse of a Matrix
## functions do

##creates a special "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(ix) invx <<- ix
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## If the inverse is ready and matrix didnt change,retrieve inverse from the cache.Otherwise compute it.

cacheSolve <- function(x, ...) {
  invx<-x$getinverse() 
  if(!is.null(invx)) {           #see if inverse is ready
      message("getting cached data")
      return(invx)
    }
  data<-x$get()                  #if not, draw x
  invx <- solve(data)
  x$setinverse(invx)
  invx ## Return a matrix that is the inverse of 'x'
}
