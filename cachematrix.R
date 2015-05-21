## Put comments here that give an overall description of what your
## functions do
## The purpose of this assignment is to write 2 functions using the new operator <<- to assign a value to an object
## in an environment that is different than the current environment. We are also expected to gain an understanding
## of some new functions,  including set and get.

## Write a short comment describing this function
## makeCacheMatrix  creates a special matrix, which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL #initialize matrix
	
	#set the value of the matrix
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	
	#get the value of the matrix
	get <- function() x
	
	#set the inverse of the matrix
	setinv <- function(solve)  m <<- solve
	
	#get the inverse of the matrix
	getinv <- function() m

	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}

## Write a short comment describing this function
## The following function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from tthe cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and set the value of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
	#get inverse of the matrix
	m <- x$getinv()

	#check to see if the matrix is null or contains cached data
	#if m is not empty return the matrix
	#if m is empty then do nothing
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	#get matrix data
	data <- x$get()
	#calculate the inverse of the matrix

	m <- solve(data, ...)
	#set inverse of matrix
	x$setinv(m)
	#print the inverse matrix
	m
}
