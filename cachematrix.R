## The following functions are able to receive a square matrix and cache its inverse
## For example, if the following command is typed: mat <- makeCacheMatrix(matrix(c(1,4,7,2,5,2,3,6,9), 3, 3))
## This is equivalent to the matrix: ( 1 2 3 )
##                                   ( 4 5 6 )
##                                   ( 7 2 9 )
## We can see that mat is a list of 4 elements: the functions get_matrix(), set_matrix(), set_inverse() and get_inverse()
##     mat$get_matrix() returns the matrix mat
##     mat$set_matrix(new_matrix) sets mat to new_matrix
##     mat$get_inverse() returns the inverse of mat (if is already calculated, else returns NULL)
##     mat$set_inverse(inv) sets the values for the inverse matrix


## Function that creates a matrix object and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set_matrix <- function(new_matrix) {
        x <<- new_matrix
        inverse <<- NULL
    }
    
    get_matrix <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
    
}


## Function that calculates the inverse of a matrix
## If the inverse has already been calculated and the matrix is unchanged, returns the inverse from the cache
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        cat("Returning cached inverse matrix:\n")
        return(inverse)
    }
    
    mtx <- x$get_matrix()
    inverse <- solve(mtx, ...)
    x$set_inverse(inverse)
    inverse
}