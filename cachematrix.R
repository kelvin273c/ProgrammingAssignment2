## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a special matrix object that can cache its own inverse
#Argument: x is a square matrix, defaults to an empty matrix, can be set later
#Return value: A list of functions, getmatrix, setmatrix, getinverse, setinverse

makeCacheMatrix <- function(x = matrix()) {
  
  #Variable matrix_inverse to store the inverse of x
  matrix_inverse <- NULL
  
  #function to set the inverse of the matrix x
  setinverse <- function(inv) matrix_inverse <<- inv
  
  #return the inverse matrix of x
  getinverse <- function() matrix_inverse
  
  
  setmatrix<-function(y) {
    x<<-y
    matrix_inverse<<-NULL
  }
  
  #Get the matrix
  getmatrix<-function() x
  
  #Return the special matrix variable as a list
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve returns the cached inverse or calculates the inverse of a special matrix x
##Argument: x is a special matrix - a list of functions and a square matrix
##Return value: Inverse of square matrix x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' if already stored in the special
  ## matrix object itself
  matrix_inverse <- x$getinverse()
  
  ## If not already cached then calculate the inverse of the matrix...
  if(!is.null(matrix_inverse)) {
    message("getting cached matrix inverse")
    return(matrix_inverse)
  }
  
  ## Calculate the inverse of x, the special matrix, and store it
  data <- x$getmatrix()
  matrix_inverse <- solve(data)
  x$setinverse(matrix_inverse)
  
  #Return the inverse of the special matrix object
  matrix_inverse
}