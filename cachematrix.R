
## Matrix Inversion

## MakeCacheMatrix creates a special 'Matrix' object that can caches its inverse

makeCacheMatrix <- function(X = matrix()) {
  ## Creates a special 'Matrix' oject and caches its inverse
  
  INV <- NULL
  set <- function(Y) {
    X <<- Y
    INV <<- NULL
  }
  get <- function() X
  
  setINV <- function(inverse) INV <<- inverse
  getINV <- function() INV
  
  cacheSolve(list(set = set, get = get,
       setINV = setINV,
       getINV = getINV))
}

## cacheSolve computes the inverse of a matrix X
      
 cacheSolve <- function(X) {
  ## Return a matrix that is the inverse of 'X'
  
  INV <- X$getINV()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  
  data <- X$get()
  INV <- solve(data)
  X$setINV(INV)
  INV
}
