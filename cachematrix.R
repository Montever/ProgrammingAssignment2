## Pair of functions to invert a square matrix 
## The inverted matrix is stored in cache so it may be retrieved without re-calculating
##
## Note: the code is based on the makeVector and cachemean functions from the coursera R course
## Example to use:
##> my_mtrx<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
##> my_special_mtrx <-makeCacheMatrix(my_mtrx)
##> cacheSolve(my_special_mtrx)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
##> cacheSolve(my_special_mtrx)
##getting cached data
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1



## Creates an object (list) to store the matrix and the inverted result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() { x }
    setinverse <- function(solve) { m <<- solve }
    getinverse <- function() { m }
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverted matrix, if the matrix had been previously calculated,
## it will returned from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
    } else {
        message("getting cached data")
    }
    m
}

