## Function makeCacheMatrix creates a special 'Matrix' object, which can cache the inverse of a provided matrix


makeCacheMatrix <- function(X = matrix()) {
	inv <- NULL
	set <- function(Y) {
		Y 	<<- Y
		inv 	<<- NULL
		}
	get <- function() X
	setinv <- function(source) inv <<- source
	getinv <- function() inv
	list(set = set, get=get, setinv=setinv, getinv=getinv)
}


## cache solve takes the output of the above function makeCacehVector and either calculates the inverse matrix and outputs,
## or if it has already been calculated it reads it from the cache to output

cacheSolve <- function(X, ...) {
  	inv <- X$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
		}
	data <- X$get()
	inv <- solve(data, ...)
	X$setinv(inv)
	inv
	}	
}
