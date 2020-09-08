## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Returns a list containing the methods to set a matrix
## and its inverse. Specificallt it contains methods to set and get
## both the matrix and its' inverse from cached memory.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {    # set the matrix and clear the inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x     # get the matrix
    setinv <- function(inverse) inv <<- inverse  # set the inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv) 
}


## Write a short comment describing this function
## Takes a makeCacheMatrix and checks if the inverse has been calculated
## If a inverse exist, return the inverse and
## else, calculate inverse, save it for the future and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # check if the inverse exists and then return the cached inverse
    if (!is.null(inv)){
        message("Getting chached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...) # calculate the inverse
    x$setinv(inv)   # save the inverse to cached memory
    inv
                     
}
