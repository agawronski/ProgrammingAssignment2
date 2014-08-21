## makeCacheMatrix ... create a list of functions allowing access to the cached matrix and its inverse
## cacheSolve ... if it exists return the cached inverse, or solve it and then return

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #assign the input matrix to x
  m <- NULL #set m to null 
  set <- function(y) { # create a sub-function which can re-set x
    x <<- y #superassign the input of this function to x
    m <<- NULL #again set m to null
  }
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

madeup <- matrix(sample(1:100, 16),4,4, byrow=TRUE)
