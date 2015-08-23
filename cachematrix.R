## There are two functions: The makeCacheMatrix that is used to create a special 
## "matrix" object
## The second fucntion, cacheSolve caches the inverse of the "matrix" object

## The makeCacheMatrix function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # store the inverse to inv
  # initialise inv
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
       x <<- y
       inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() {
      x
  }
  
  # set the value of the cached inverse matrix
      setinverse <- function(inverse) {
      inv <<- inverse
    }
  
  # get the value of the cached inverse matrix
    getinverse <- function() {
      inv
  }
  
  # return the special matrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## The cacheSolve fuction takes the special "matrix" object created by the 
## makeCacheMatrix function and calculates its inverse.  However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached inverse
  inv <- x$getinverse()
  
  # check if the inverse has been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  
  # otherwise, calculate the inverse and cache it
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setinverse(inv)
  return(inv)
 
  
}
