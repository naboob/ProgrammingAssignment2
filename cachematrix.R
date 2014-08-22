##  This function creates a special "matrix" object that 
##  can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    
    ## get the inverse of the matrix
    getinverse <- function() m
    
    ## save the informations in a list to pass it to the 
    ## cacheSolve.R function
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix().  
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    ## the information generated from makeCachMatrix.R is loaded
    ## into m
    
    m <- x$getinverse()
    
    ## If the inverse has already
    ## been calculated (and the matrix has not changed)
    ## then the cachesolve should retrieve the inverse 
    ## from the cache.
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


