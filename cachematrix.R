## The two functions "makeCacheMatrix" and "cacheSolve" can be used to calculate
## the invers of a matrix and to store the value in cache. Instead of recalculating
## the value again it is taken from cache the second time it is needed 

## The first function, makeCacheMatrix creates a list containing four functions:
## 1. sets the value of the matrix "x"
## 2. gets the value of the matrix "x"
## 3. sets the value of the invers matrix "i"
## 4. gets the value of the invers matrix "i"

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                       # sets the value of the matrix "x"
        x <<- y                                # value of matrix "i" is set to 0,
        i <<- NULL                             # so that "i" is not taken from cache
    }                                          # during first calculation when "x" changes
    
    get <- function() x                        # gets the value of the matrix "x"
    setinvers <- function(invers) i <<- invers # sets the value of the invers matrix "i"
    getinvers <- function() i                  # gets the value of the invers matrix "i"
    list(set = set, get = get,                 # creates list containing the four functions
         setinvers = setinvers,
         getinvers = getinvers)
}

## The second function, cacheSolve calculates a matrix "i" that is the inverse of matrix "x",
## from the vector created by the "makeCacheMatrix" function. If "i" was calculated before it
## takes the data from the cache and skips computation. Otherwise, it calculates "i" and sets
## the value in the cache via the "setinvers" function.

cacheSolve <- function(x, ...) {
    i <- x$getinvers()                   # gets the value of the invers matrix "i"
    
    if(!is.null(i)) {                    # inverse of matrix "x" is taken from cache
        message("getting cached data")   # and printed, if it was calculated before
        return(i)                        # unless the input matrix was changed by "set" function
    }
    data <- x$get()                      # gets the value of the matrix "x"
    i <- solve(data, ...)                # calculates the inverse of the matrix "x"
    x$setinvers(i)                       # sets the value of the invers matrix "i"
    message("calculating data")
    i                                    # prints calculated matrix "i"
}