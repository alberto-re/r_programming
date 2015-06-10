# This function returns a list of four functions operating on a matrix:
#
# set        - sets the matrix object
# get        - gets the matrix object
# setinverse - sets the inverse of the matrix
# getinverse - gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() { x }

        setinverse <- function(y) { inverse <<- y }

        getinverse <- function() { inverse }

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of a matrix.
# Matrix parameter must be passed as a list previously obtained with `makeCacheMatrix`.
# If the inverse has been already calculated it returns the cached value, otherwise
# the solve() method is performed on the fly and the value is stored in cache for
# any subsequent call.
#
# This method assumes that the matrix given is invertible.
cacheSolve <- function(x) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
