
# The first function, makeCacheMatrix creates a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
#This function creates a matrix that can cache it's inverse

# Args:
#   x: A matrix (Optional)
#
# Returns: A matrix with functions to get or set the value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  ##Creates a matrix that can cache it's inverse
  #Cached inverse of matrix set to null
  inv <- NULL
  
  #Define get/set for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Define get/set for inverse of matrix
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  
  #Return list of functions for matrix and inverse
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}

#The following function calculates the inverse of the special matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in
#the cache via the setinverseerse function.

# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns: The inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("getting cached data of inverse")
    return(inv)
  }
  
  #Calculate inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  #Set cache inverse
  x$setinverse(inv)
  
  #Returns inverse of matrix
  return(inv)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created a 2 X 2 matrix
# m <- matrix(c(10, 20, 5, 4), nrow = 2, ncol = 2, byrow = TRUE)
# m2 <- makeCacheMatrix(m)
# cacheSolve(m2)
#            [,1]       [,2]
#[1,] -0.06666667  0.3333333
#[2,]  0.08333333 -0.1666667
#Call again cacheSolve(m2)
# cacheSolve(m2) 
# getting cached data of inverse
#            [,1]       [,2]
#[1,] -0.06666667  0.3333333
#[2,]  0.08333333 -0.1666667
