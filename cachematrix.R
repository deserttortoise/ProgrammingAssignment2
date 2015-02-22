## makeCacheMatrix Creates a list of functions to be passed 
##to the  Cache solve function
makeCacheMatrix <- function(x = matrix()) {
  
  
  #initializes the variable m
  m <- NULL
  
  ## uses the << to create a global variable y and
  ## set any previous global variable m to NUll
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  ## the output a list of 4 functions:set,get,setinv,getinv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrieves the inverse matrix from the global
## environment. If no matrix is found it inverts the matrix
cacheSolve <- function(x, ...) {
  
  ## retrieve the inverse matrix m and display message
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if can't find the inverse matrix then solve the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}