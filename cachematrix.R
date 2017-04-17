## Function makeCacheMatrix creates a special matrix 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  InvM <- NULL
  set <- function(y) {
    x <<- y
    InvM <<- NULL
  }
  get <- function() x
  setInvM <- function(solve) InvM <<- solve
  getInvM <- function() InvM
  list(set = set, get = get,setInvM = setInvM, getInvM = getInvM)
  
}


## The CacheSolve function computes the inverse of the special matrix 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  InvM <- x$getInvM()
  if(!is.null(InvM)) {
    message("getting cached data")
    return(InvM)
  }
  data <- x$get()
  InvM <- solve(data)
  x$setInvM(InvM)
  InvM   ## Return a matrix that is the inverse of 'x'
}