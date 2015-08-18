
## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse. 

## See below how they can be tested.



## The function makeCacheMatrix creates a special "matrix", which
## is really a list containing a function to

##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse
##	4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}



## It can be tested like this:

## m <- makeCacheMatrix(matrixA) ## where matrixA is a matrix previously
                                 ## created

## cacheSolve(m)                 ## The inverse is calculated, so the message
                                 ## "getting cached data" is not printed

## cacheSolve(m)                 ## The inverse was in the cache, so it is
                                 ## not calculated this time, so the message
                                 ## "getting cached data" is printed

## m$set(matrixB)                ## where matrixB is another matrix
                                 ## previously created

## cacheSolve(m)                 ## The inverse has to be calculated again,
                                 ## so the message "getting cached data" is
                                 ## not printed

## cacheSolve(m)                 ## The inverse was in the cache, so it is
                                 ## not calculated this time, so the message
                                 ## "getting cached data" is printed
