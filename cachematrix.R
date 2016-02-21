## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Tested the function with data <- matrix (c(2,4,0,3,3,1,4,2,0),ncol = 3)
## The next function will create a matrix object, which it is a list of functions, which stores its own matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##This function receives the object matrix from last function.
##When this type of special objects gets to this function, it will review
##if it is need to apply solve function or bring the inverse matrix from the 
##matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        message("calculating inverse matrix")
        x$setinverse(m)
        message("Inverse matrix added to cache")
        m
}

