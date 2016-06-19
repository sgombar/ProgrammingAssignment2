## Goal: Make a class that can store the inverse of a matrix and return it to prevent long re-compute times
## 2 functions: one to wrap the matrix and its inverse; (set and get); the other to compute the inverse or pull a previously computed one

## This funciton creates this matrix wrapper and provides set and get functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	
	setinv <- function(inverse) inv <<- inverse 
	
	getinv <- function() inv
	
	list(set = set, get = get, setinv=setinv, getinv=getinv)
}


## This function checks if the inverse of this matrix exists; if so it returns it. else it calculates and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		
		if(!is.null(inv)) {
			message("Getting Cached Inverse")
			return(inv)
		}
		
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
