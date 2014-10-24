## Programming Assignment 2
## Week 3
## Create two functions that will facilitate the creation of a matrix and the 
## calculation of it's inverse. For performace the inverse should be cachable.

## makeCacheMatrix
## This is a constructor function which will store and retrieve a matrix and it's inverse.
## Allows the setting and getting of the matrix and the inverse

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


## cacheSolve consumes the function created by makeCacheMatrix and calculates the inverse of matrix.
## The inverse will only be calculated if it has not already been done so. 
## If the inverse has already been calculated then the pre-calculated inverse matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) { ## Do we have a cached version?
                return(inv) ## Yes - return the cached version
        }
        
        ## - No we don't have a cached version - calc, store and return the inverse.
        data <- x$get()
        inv <- solve(data)         
        x$setinverse(inv)
        inv
}
