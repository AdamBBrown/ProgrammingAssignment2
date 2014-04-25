## These are two functions for the R Programming Coursera code. They are designed to show the programmer how to access
## objects in an environment that is different from the current one.
## Author: Adam B. Brown
## Date: April 15, 2014
## Version: 1.0

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## it stores the globalMatrix and the globalInverseMatrix using the <<- keyword so that they are accessible from other environments.
makeCacheMatrix <- function(globalMatrix = matrix()) {
        #check that x is a matrix()
        if(!typeof(globalMatrix) == "matrix" ) stop("globalMatrix argument must be an object of type matrix")
        globalInverseMatrix <- NULL #reset the cached copy of the inverse matrix when invoking the function
        setMatrix <- function(localMatrix) {
                globalMatrix <<- localMatrix #set the global matrix to the new matrix passed into the function
                globalInverseMatrix <<- NULL #erase any cached copies of the inverse matrix
        }
        getMatrix <- function() globalMatrix #return the directmatrix
        setInverseMatrix <- function(localInverseMatrix) globalInverseMatrix <<- localInverseMatrix #set the cached copy of inversematrix
        getInverseMatrix <- function() globalInverseMatrix #return the cached copy of the inversematrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## cacheSolve: this function first looks in another environment to see if the inverse of the matrix created with the makeCacheMatrix
## function, then if it's not already cached, it  creates the inverse of the matrix stored makeCacheMatrix function and caches it.
cacheSolve <- function(cacheMatrixFunction, ...) {
        #look for the inverse matrix in the cache
        returnInverseMatrix <- cacheMatrixFunction$getInverseMatrix()
        if(!is.null(returnInverseMatrix)) {
                message("getting cached data") #show message indicating we are looking at the cached version
                return(returnInverseMatrix) #return the cached copy of the inverse matrix
        }
        forwardMatrix <- cacheMatrixFunction$getMatrix() #if it's not in cache, get the direct matrix from the makeCacheMatrix function
        returnInverseMatrix <- solve(forwardMatrix) #reverse the matrix using the solve() function
        cacheMatrixFunction$setInverseMatrix(returnInverseMatrix) #store the inversematrix in the cache
        returnInverseMatrix #return the inversematrix
}
