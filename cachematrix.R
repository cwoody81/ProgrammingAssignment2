## The overall function is caching the inverse of a matrix
## The first function is a constructor function where you are creating a matrix 
## and assigning it to a variable inside the global environment.
## The function first clears the global environment of any stored data assigned
## to that variable.  Then establishes variables that are functions to be called
## in the cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<- y
    inv<<- NULL
    }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
  }


## This function will test if their is a stored value in the cache for x, if 
##if there is it will return the cache value. If not it will solve for the 
##inverse, store it in the cache, and display the inverse matrix.


cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
  
   ## Return a matrix that is the inverse of 'x'
}
