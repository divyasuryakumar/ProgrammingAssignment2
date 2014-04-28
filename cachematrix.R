## [The makeCacheMatrix function will get an input matrix, save the matrix as a list 
## and find the inverse of the matrix and sets that value of the inverse of the matrix in Cache. 
## The get cacheSolve function will check to see if the cache is empty and if it is not then 
## it will pull the contents from the Cache and pass it to Inv and will print it on the screen.]

makeCacheMatrix <- function(x=matrix()) {
	
	Inv <- NULL
  	set <- function(y) {
    		x <<- y
    		Inv <<- NULL
  	}
  	get <- function() x
  	setInv <- function(k) Inv <<- k
  	getInv <- function() Inv
  	list(set = set, get = get, setInv = setInv, getInv = getInv)

}

## when this function is called it first check if the cache is empty if its not empty then get the contents from
## cached matrix and finds the inverse using the solve function. 

cacheSolve <- function(x, ...) {
 
  	Inv <- x$getInv()
  	if(!is.null(Inv)) {
    		message("getting cached data")
    		return(Inv)
  	}
  	data <- x$get()
  	Inv <- solve(data)
  	x$setInv(Inv)
  	Inv

}



