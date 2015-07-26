## Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly
# (there are also alternatives to matrix inversion that we will not discuss
# here). Your assignment is to write a pair of functions that cache the inverse
# of a matrix.

## Write the following functions:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function
# in R. For example, if X is a square invertible matrix, then solve(X) returns
# its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

## R matrix operations described here: (specifically "solve()"
# http://www.statmethods.net/advstats/matrix.html

# some invertable matrix found on-ine: (I googled it, but don't recall the URL)
# x = c(2, 3, 1, 5, 1, 0, 3, 1, 0, 2, -3, 2, 0, 2, 3, 1 )
# m = x1 = matrix(x, nrow=4, ncol=4)




## Creates a matrix object that holds a matrix and has space to store its inverse.
# The commented out lines are those that were removed from the original example
# function for computing the mean of a vector.
# That example function can be found here: 
# https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions

# makeVector <- function(x = numeric()) {
makeCacheMatrix <- function(x = numeric()) { 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # setmean <- function(mean) m <<- mean
    setInverse <- function(inverse) m <<- inverse
    # getmean <- function() m
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    # setmean = setmean, getmean = getmean)
}


## Returns the inverse of x by either computing it or by finding it in the cached version of the matrix
# The commented out lines are those that were removed from the original example
# function for computing the mean of a vector.
# That example function can be found here:
# https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions

# cachemean <- function(x, ...) {
cachesolve <- function(x, ...) {
    # m <- x$getmean()
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # m <- mean(data, ...)
    m <- solve(data) ## THis line breaks if you include the ", ...)" from the mean function,
    # which seems odd since there is an elipsis argument to that function in the help files. 
    x$setInverse(m)
    m
}
