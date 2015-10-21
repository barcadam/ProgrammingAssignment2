## The following 2 functions serve to cache an inverse matrix, thus saving
## on future computational time.
## The first function does the heavy lifing including calculating the inverse
## and providing the functionality of caching the inverse.
## The second function determines if the inverse has been cached and then
## calls the appropriate function to retrive or calculate and cache 
## the inverse.


## This function creates a matrix object that can cache its inverse.
## The main function contains 4 sub functions, that can be callled
## once a "makeCacheMatrix" object is initialized.  The 4 sub functions provide
## the ability to get and set a matrix and get and set the inverse of the
## matrix object.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(matrix_to_inverse) {
        inverse <- solve(matrix_to_inverse)
    }
    getInverse <- function() inverse
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
##  utilizing the embedded functions of the makeCacheMatrix object.
## If the inverse has already been calculated & cached this function 
## retrieves the inverse from the cache.
## Otherwise, if the inverse has not yet been calculated and cashed, 
## then it will call on the getInverse function from the makeCacheMatrix object.
cacheSolve <- function(x, ...) {
    inverseMatrix <- makeCacheMatrix(x)
    inverse <- inverseMatrix$getInverse()
    if(!isNull(inverse)) {
        message("getting cashed matrix")
        return(inverse)
    }
    inverse <- inverseMatrix$setInverse(x)
}