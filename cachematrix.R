## This set of functions caches and retrieves the inverse of a matrix

## The first function sets up four other functions, which set the
## matrix, get it back, set the inverse, and get it back. The
## output is a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function () inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The second function checks to see if the inverse is already cached.
## If so, it simply returns the cached inverse matrix; otherwise it 
## calculates the inverse, puts it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
