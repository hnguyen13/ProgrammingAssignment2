## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse
## get is a function that returns the matrix x in the makeCacheMatrix function. 
## set is a function that changes the matrix in the makeCacheMatrix function.
## setinverse and getinverse will set and return the the inverse matrix of x.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i<<- inverse
	getinverse <- function() i
	list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve is a function to return the inverse matrix of the matrix
## in the argument
## cacheSolve first calls getinverse to see if there is an inverse matrix
## and not null.  If there is, it will say "getting cache data"
## else, it would return an inverse matrix by calling solve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return (i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
