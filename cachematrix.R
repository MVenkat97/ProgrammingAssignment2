## Below function will cache inverse of a matrix
## Matrix inversion is usually costly computation, instead of repeatedly compting it,
##it is better to cache the inverse using below functions.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv<-NULL
    set<-function(y){
    x<<-y
    inv<<-NULL
}

get<-function() x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function() inv
list (set =set,
      get=get,
      setinverse=setinverse,
      getinverse=getinverse)

}


## This function computes inverse of the special "matrix" created by
##makeCacheMatrix above. If the inverse has already been calculated(
##and matrix has not changed),then it should retrieve matrix from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv<-x$getinverse()
	if(!is.null(inv)){
	message("getting cached data")
	return(inv)
	}
	mat<-x$get()
	inv<-solve(mat, ...)
	x$setinverse(inv)
	inv
}




