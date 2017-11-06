## The make "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## This function sets the value of matrix, gets the value of the matrix, sets the inverse and then gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    
    m <<- NULL 
  }

  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m

cacheSolve <- function(x) {
  m <- x$getInverse() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m                
}
