## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    
    ## set the matix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    ## get the matix
    get <- function() x
    
    ## set the inverse
    setInv <- function(inverse) invMatrix <- inverse
    
    ## get the inverse
    getInv <- function() inverse
    
    ## return the list of functions
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## get the inverse of the matrix
    inv <- x$getInv()
    
    ## check it has a value
    if (!is.null(inv)){
        ## if it has a value, return it 
        message("getting cached matrix")
        return(inv)
    }
    
    ## if it doesn't have a value, get the matix and calculate the inverse 
    matixData <- x$get()
    inv <- solve(matrixData, ...)
    
    ## set the inverse in the cache
    x$setInv(inv)
    
    return(inv)
}
