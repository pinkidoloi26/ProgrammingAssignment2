## ProgrammingAssignment2
## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse
## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above

## makeCacheMatrix creates and returns a list of functions, it is used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                             # stores the cache value and initializes to NULL
        set <- function(y){                     # this function creates the matrix in working environment
                x <<- y
                inv <<- NULL
                }
        get <- function()x                      # this function gets the value of the matrix in working environment
        setinv <-function(inverse)inv<<-inverse #invert the matrix and stores in the cache
        getinv <- function()inv                 #it gets the inverted matrix from the cache
        list(set = set, get = get, setinv = setinv, getinv = getinv) #return the created function to the working environment

}


## CacheSolve calculates the inverse of the matrix created in makeCacheMatrix, this function is stored in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()                  # attempt to get the inverse of the matrix stored in cache
        if(!is.null(inv)){                     # return inverted matrix from cache if it exist
                message("getting cached data") # display matrix in console
                return(inv)
                }
        mat <- x$get()                         # create matrix since it does not exist
        inv <- solve(mat, ...)                 # set and return inverse of matrix
        x$setInverse(inv)                      # set inverted matrix in cache
        inv                                    # display matrix in console
}
