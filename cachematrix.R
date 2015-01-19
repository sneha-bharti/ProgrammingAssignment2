# makeCacheMatrix - creates function to:
# - set the value of matrix
# - get the value of matrix
# - set the value of inverse
# - get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInv <- function(matrix) inverse <<- matrix
        getInv <- function() inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

#The following function calculates the inverse of the matrix 
# First it checks to see if the inverse is already there
# If the inverse exists, it gets the inverse from the cache and returns.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in tme makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
