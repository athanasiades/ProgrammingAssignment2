## Put comments here that give an overall description of what your
## functions do
## The examples are generic enough that support any object.
## To complete the assignment, one has to just remplace mean with solve
## i.e. change the operation. The new object is defined at the definition
## of makeCacheMatrix.

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix which can be cached.
## It adds the get / set operations for the matrix and the solve operation into
## a list.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve takes the list of get/set operations for the matrix and solve.
## The function attemtpts to get the inverse matrix. If It is was calculated 
## before, it returns the cached version, otherwise it get the data and 
## calculates the inverse matrix and stores it. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

