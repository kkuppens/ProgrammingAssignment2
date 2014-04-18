## The makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
	# Initialize to be cached inversed matrix 
  	inv_matrix <- NULL
  
  	# function to set the value of the matrix
  	set <- function(y) {
    		x <<- y
    		inv_matrix <<- NULL #New matrix, build new cache
  	}

  	# function to get the value of the matrix
  	get <- function() x

  	# function to set the inverse of the matrix
  	setinverse <- function(p_inv_matrix) inv_matrix <<- p_inv_matrix

  	# function to get the inversed cached matrix
  	getinverse <- function() inv_matrix
  
  	# return a list of all the above functions
  	list(set = set, get = get, setinverse = setinverse, 
		getinverse = getinverse)
}

## The cacheSolve function calculates the inversed matrix of 
## the special "matrix" created with the makeCacheMatrix function. 
## Before calculating the inversed matrix it will first check the presence of 
## the inversed matrix in the cache of the computer.
## In case the inversed matrix exists in cache it will return the  
## inversed matrix present in cache, otherwise the solve fucntion will be 
## used to calculate the inversed matrix, result will be cached and returned

cacheSolve <- function(x, ...) {
	# check if the inversed matrix has already been cached
	inverse <- x$getinverse()
  	if(!is.null(inverse)) {
    		message("getting cached Inversed Matrix data")
    		# The inverse of the matrix has been cached
		# return immediate result
    		return(inverse)
  	}
  	# no cached inverse matrix found, get special matrix
  	matrix <- x$get()
  	# call the solve function to calculate the inversed matrix
  	inverse <- solve(matrix, ...)
  	# cache the inversed matrix
  	x$setinverse(inverse)
  	# return the inversed matrix 
  	inverse
}

## 
