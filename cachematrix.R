
## Running matrix inverse computation can be time consuming, so it would 
## save time to cache the result if the same result will need to be used 
## often later. The two functions below is to cache the inverse of a 
## special matrix.

## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse, using solve function.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverseMatrix <<- solve
    getinverse <- function() inverseMatrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getinverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setinverse(inverseMatrix)
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
