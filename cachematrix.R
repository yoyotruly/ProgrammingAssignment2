## Coursera R Programming Assignment 2 
## Write a pair of functions that cache the inverse of a matrix rather than
## compute it repeatedly

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by the function above
## If the inverse has already been calculated and the matrix did not change,
## then retrieve the cached value

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)) {
        message("getting cached data")
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Test both functions

set.seed(1)
test <- makeCacheMatrix(matrix(rpois(9, 1), 3, 3))
cacheSolve(test)
cacheSolve(test)
