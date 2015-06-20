makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
   set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  get <- function() {
    x
  }
  
  
  setinv <- function(i) {
    inv <<- i
  }
  
  
  getinv <- function() {
    inv
  }
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
}



cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    
    message("getting cached inverse")
    return(inv)
  }
  
  
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  
  return(inv)
}
