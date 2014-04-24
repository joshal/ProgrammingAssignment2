## Calculating the inverse of a matrix is a costly computation.
## The functions in this script provide a way to cache the inverse of a matrix during the
## first calculation and return the cached result with subsequent calls made to
## fetch the inverse of the same matrix
## 
## Example Usage:
## > A <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## > cachedMatrix <- makeCacheMatrix(A)
## > cacheSolve(cachedMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cachedMatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## The `makeCacheMatrix` function takes in a matrix as a input and returns a
## list of getter and setter functions to cache and retrieve the data and the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The `cacheSolve` function takes in the list obtained from the
## `makeCacheMatrix` function, checks if the inverse if already calculated, and
## returns the cached result to skip the calculation if the inverse is obtained or
## calculates and stores the inverse which is used with subsequent calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
