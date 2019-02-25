## These two functions are similar to the ones given in the example.
## The idea is basically the same: save time and calculation power when the inverse
## is already calculated. In order to accomplish this task, I have used lexical scoping,
## which lets you work with different environments and a more flexible use of the memory

## This function creates a list of 4 objects, each of one is another function. 
## Specifically, these functions are the "getters" and the "setters" of a typical object-
## oriented language. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inver) inv <<- inver
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The possibility of working with different environments allows the function "cacheSolve"
## to access the data in the other function, as this data belongs to de global
## environment

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}