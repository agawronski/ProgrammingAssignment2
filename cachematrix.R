# These two functions will allow for the inverse of a matrix to be stored
# in order to save computation if it needs to be called again
# instead of using solve() (when trying to find the inverse of a matrix) we will
# use our cacheSolve() function...this will first check the object which has been 
# created using makeCacheMatrix and determine whether the inverse has been stored.
# If not it will access the data (in the makeCacheMatrix object) and then solve the 
# inverse and store it for future access.

# This function will create a list object with the list elements being functions
# makeCacheMatrix will initally set variable m (our cache) to null
# the functions in the list will be accessible to the cacheSolve function
# allowing cache solve to get the input matrix, store the inverse matrix in m, or
# access the inverse matrix if it has already been stored.

makeCacheMatrix <- function(x = matrix()) { #input x will be a matrix object or running cacheSolve will fail
  
  m <- NULL # reset our cache for the inverse to NULL each time makeCacheMatrix is called

  get <- function() x # this function allows cacheSolve to access the input matrix if our cache m is empty (NULL) 
                      # ... that is if it is the first time we are running cacheSolve
  
  setinverse <- function(inverse) m <<- inverse # everytime we run cacheSolve this 
                                                # function will superassign the inverse matrix to m
                                                # the superassignment allows the previous NULL (or previous 
                                                # inverse matrix) to be overwritten
  
  getinverse <- function() m # This function will allow cacheSolve to access the stored inverse (if it is there)

  list(get = get, # This creates a list of the functions above and names them for access to cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function will check if the inverse has been stored and return it if it has
## Otherwise it will get the matrix data, compute the inverse, and cache the inverse storing it in m

cacheSolve <- function(x, ...) { # the input wil be the object created by makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() # this assigns to m whetever was stored in m in the list object (either NULL or the inverse matrix)
  if(!is.null(m)) { # check if m is null, and if it is not then return the following
    message("getting cached inverse matrix") # return the message "getting cached inverse"
    return(m) # then return the inverse matrix
  }
  data <- x$get() # this code (and below) only runs if m is NULL, it pulls the matrix data and assigns it to "data"
  m <- solve(data, ...) # this line calculates the inverse using solve and assigns it to m
  x$setinverse(m) # this line stores the inverse in m within the list object environment by using the setinverse function (superassigning m)
  m #finally it returns the inverse matrix
}
