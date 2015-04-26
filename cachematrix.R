# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invs <- NULL
	set <- function(y){
		x <<- y
		invs <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data.")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinverse(invs)
    invs
}

