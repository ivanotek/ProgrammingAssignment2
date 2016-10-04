#' 
#' Function “makeCacheMatrix” creates a matrix container object, that store a matrix and 
#' caches its inverse. 
#' 
#' makeCacheMatrix contains 4 functions: set, get, setInverse, getInverse.
#' 
#' set() is a function that changes the matrix stored. It clears the cached inverse for
#'      consistency.
#' get() is a function that returns the matrix stored.
#' setInverse() is a function that changes the cached inverse matrix
#' getInverse() is a function that returns the cached inverse matrix
#' 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse<<-inv
    getInverse <- function() inverse
    list(get=get, 
         set=set, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


#' 
#' Function “cacheSolve” handles a matrix container object and returns the inverse of 
#' its contained matrix m. If has not been evaluated previously, evaluates the inverse 
#' of m and stores the result in the container with x$setInverse(). 
#' Subsequent calls to container returns the previously evaluated inverse of m with 
#' x$getInverse(). 
#' 
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)
    inv
}
