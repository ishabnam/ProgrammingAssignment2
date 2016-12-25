## Put comments here that give an overall description of what your
## functions do
# We rely on the <<- operator to assign the inversion of a matrix first and
## later, use it to avoid repeated calculations.
# In what follows, we introduce two functions: (1) makeCacheMatrix which
# creates a list containing the cached inversion of a matrix (x).
# (2) cacheSolve(x, ...) which uses the created list in an efficient way.
# example of use:
# x <- matrix(rnorm(9), 3, 3)
# y <- makeCacheMatrix(x)
# for (i in 1:3) {cacheSolve(y)}


## Write a short comment describing this function
# Creates a special matrix object which is a list containing functions to 
# (i) set the value of the matrix
# (ii) get the value of the matrix. 
# (iii) set the value of the invert of the matrix
# (iv) get the value of the invert of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL 
        }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# calculates the inverse of the special matrix object created with the above
# function. First, we check to see if the invert has already been calculated.
# If yes, we simply get the invert from the cache and skip the computation. 
# Otherwise, we calculate the invert of the matrix and set this value 
# for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}