## Description: The two functions first caches the inverse of a matrix in the memory, then when the matrix has not changed, the cached inverse would be retrieved. 
## Programmer: Yi Wang
## Date: 6/21/2015

## This function created a special "matrix" object that cache its inverse, which is really a list containing a function to set/get the value of the matrix, and set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set<-function(y){
		x<<-y
		s<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) s<<-solve
	getinverse<-function() s
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" created with the function makeCacheMatrix above. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skip the computation, otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	s<-x$getinverse()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data<-x$get()
	s<-solve(data,...)
	x$setinverse(s)
	s
}
