## Put comments here that give an overall description of what your
## functions do

## This function is a set of functions which assigns matrix value to a matrix object and inverse of the matrix to another matrix object.
## And contains the functions which returns the matrix object and inverse matrix object. 
## makeCacheMatrix()  is the main function which returns the list of all the function inside it.
## makeCacheMatrix()$setmatrix(matrix(1:4,  2,   2))  -- Assigning value to a matrix object
## makeCacheMatrix()$getInverseMatrix() -- calculating Inverse matrix and assigning it to a matrix object
## Version 1

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  setmatrix <- function(passedmat) {
    x <<- passedmat
    imat <<- NULL
  }
  getmatrix <- function() x
  setInverseMatrix <- function(ComputedIMat) imat <<- ComputedIMat
  getInverseMatrix <- function() imat
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## This function resolves the cache of an Inverse matrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InverseMatrix  <- x$getInverseMatrix()
  if(!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
  data <- x$getmatrix()
  imat <- solve(data, ...)
  x$setInverseMatrix(imat)
  imat

}
