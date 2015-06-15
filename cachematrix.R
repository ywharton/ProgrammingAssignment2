## creates a special object that stores a matrix and caches it's inverse
## assumes that the matrix supplied is always invertible.


## makeCacheMatrix
## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## cacheSolve
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

##> I<-diag(5)
##> I
##      [,1] [,2] [,3] [,4] [,5]
##[1,]    1    0    0    0    0
##[2,]    0    1    0    0    0
##[3,]    0    0    1    0    0
##[4,]    0    0    0    1    0
##[5,]    0    0    0    0    1
##> D<-makeCacheMatrix(I)
##> D$get()
##      [,1] [,2] [,3] [,4] [,5]
##[1,]    1    0    0    0    0
##[2,]    0    1    0    0    0
##[3,]    0    0    1    0    0
##[4,]    0    0    0    1    0
##[5,]    0    0    0    0    1
##> cacheSolve(D)
##      [,1] [,2] [,3] [,4] [,5]
##[1,]    1    0    0    0    0
##[2,]    0    1    0    0    0
##[3,]    0    0    1    0    0
##[4,]    0    0    0    1    0
##[5,]    0    0    0    0    1
##> cacheSolve(D)
##getting cached data
##      [,1] [,2] [,3] [,4] [,5]
##[1,]    1    0    0    0    0
##[2,]    0    1    0    0    0
##[3,]    0    0    1    0    0
##[4,]    0    0    0    1    0
##[5,]    0    0    0    0    1

