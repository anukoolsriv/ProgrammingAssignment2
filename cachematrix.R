## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix consists of 4 functions
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL    
  
  set <- function(y) {  # it sets the value of the matrix and inverse to null
    x <<- y
    inverse <<- NULL
  }
  
  get <- function(){    # it gets the value of the matrix
    x
  } 
  setinverse <- function(){ # it calculates the inverse to the matrix and
    inverse <<- solve(x)
  } 
  
  getinverse <- function(){    # function to extract the inverse of the matrix
    inverse
  } 
  
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  # list of all the fucntions
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()  # it calss to extract the inverse
    if(!is.null(inverse)) {  # checks if inverse exists in the memory(cache)
      message("getting the cache matrix")
      return(inverse)        # extracts it
    }
  
  invMat <- inverse$get()    # get the inverseMatrix and store in a variable
  inverse <- solve(invMat, ...) # solves the matrix to calculate inverse
  
  inverse$setinverse(inverse)   # sets the inverse of the matrix
    inverse            # returns inverse
}