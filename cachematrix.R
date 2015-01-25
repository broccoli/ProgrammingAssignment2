
## Return a matrix container object that can cache its inverse.
## Use with a caching function.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}



## Return a matrix inverse from a matrix container.
## Function caches the inverse in the container.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
