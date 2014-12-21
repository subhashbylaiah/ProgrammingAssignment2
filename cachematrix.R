## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a matrix object and defines the getter/setter 
## methods for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  xinv = NULL #initialze the matix inverse as NULL
  
  #the set function will reset the variables for the object,
  # sets the matric to the parameter passed in
  # initializes the inverse to NULL
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() { x }  #get will just return the matrix variable
  
  setinv <- function(inv) { xinv <<- inv } # this is setting the inverse of the matrix
  
  getinv <- function() { xinv } # this is returning the inverse
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## cacheSolve function will compute the inverse of the matrix
## if not computed already
## if its already computed, it will return the inverse in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
  
}
