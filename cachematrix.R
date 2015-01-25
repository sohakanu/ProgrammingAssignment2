## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix finds the inverse of the matrix
## and cache the value in the variable 'inv'. 
## 'set' function assigns the value 'y' to matrix 'x' and clears the 
## cached inverse value
## The 'get' and 'getInverse' return the matrix and the inverse respectively 

makeCacheMatrix <- function(x = matrix()) {

	inv <- solve(x)
	
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function(){
		x
	}
	
	setInverse <- function(newInv){
		inv <<- newInv
	}
	
	getInverse <- function(){
		inv
	}
	
	
	list(set = set, get = get,  setInverse = setInverse, getInverse = getInverse)
}


## The function checks if the object inverse is has a value, if yes it imples that 
## the matrix is unchanged and the returns the cached inverse. if the value has on the 
## inverse is null, the inverse is calculated and cached in the object 

cacheSolve <- function(x, ...) {

	inv <- x$getInverse()
	
	if( !is.null(inv)){
		return (inv)
		
	}
	
	else {
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
		return(inv)
		
	}
       
}
