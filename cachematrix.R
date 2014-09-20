## Put comments here that give an overall description of what your
## functions do
## `makeCacheMatrix` creates a special "matrix" object that can cache its inverse.


##  'makeCacheMatrix' returns a list of the following functions:
##  1.  set: set the value of the matrix
##  2.  get: get the value of the matrix
##  3.  setinverse: set the value of the matrix inverse
##  4.  getinverse: get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv<<-inverse
    
    getinverse <- function() inv
    
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    
    if(!is.null(inv)){
        message("getting cached inverse value")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
}
