## this function creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set<-function(y){
        x<<-y
        s<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) s<<- solve
    getmatrix<-function() s
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## The following function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve 
## vthe inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    s<-x$getmatrix()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    matrix<-x$get()
    s<-solve(matrix, ...)
    x$setmatrix(s)
    s
}

