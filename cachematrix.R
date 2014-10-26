## Since geting the inverse of a matrix can be costly, this couple of functions 
## can cache the result of the operation for future recalling. 

## This function makes a list with 4 functions
## 1) set(x) -> sets the value of the matrix
## 2) get() -> returns it's value
## 3) setinv(inv) -> sets it's inverse
## 4) getinv() -> gets it's inverse
## In other words, it can be thought as an element wich has a matrix
## and -if it was computed- it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) inv <<- y
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a matrix. If it was computed
## and stored in the list, it will return the cached version. If not, it will
## compute it and then cache it. 

cacheSolve <- function(x, ...) {
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
