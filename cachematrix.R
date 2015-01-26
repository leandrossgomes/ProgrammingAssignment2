## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## Part 1: The first function, makeCacheMatrix creates a special "Matrix"


makeCacheMatrix <- function(x = matrix()) {             # sets x equal to an empty matrix
        inv <- NULL                                     #initialize the inverse matrix as null
        set <- function(y) {
                x <<- y                                 # set function assigns the matrix to x
                inv <<- NULL                            # When set function is called, Inverse is re-set to NULL. 
        }
        get <- function() x                             # get function returns the matrix x
        setinv <- function(solve) inv <<-- solve        # setinv assign the supposed inverse of matrix x to I
        getinv <- function() inv                        # getinv returns the inverse matrix
        list (set = set, 
              get = get, 
              setinv = setinv,
              getinv = getinv)                          # create a list of functions 
}
## Part 2: The seconf function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## In case of the inverse has already been calculated for the same matrix before, the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                               # Returns the value for the inverse
        if(!is.null(inv)){                              # If the value for the inverse is not null, this value will be returned
                message("getting cached data")
                return(inv)
        }
        mdata <- x$get()                                # If the value of the inverse is null then the matrix x wil be retrieved
        inv <- solve(mdata, ...)                        # The inverse of the matrix is computed 
        x$setinv(inv)                                   # The inverse value is assigned into memory
        inv                                             # Then the new value of the inverse is returned
}