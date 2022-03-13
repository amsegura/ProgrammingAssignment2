## Programming assignment2
## A function to create a matrix, compute and cache its inverse is created.

## This function creates a the special "matrix" object that can cached its
## inverse.
makeCacheMatrix <- function(x = matrix()) {
        i2<- NULL
        set<- function(y){
                x<<-y
                i2<<-NULL
        }
        get<- function() x
        setinv<-function(inv) i2<<- inv
        getinv<- function() i2
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if the matrix passed has already an inverse calculated.
## If so, retrieves from cache that value. If not, calculates and cached the 
## inverse of that matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i2<- x$getinv()
        if(!is.null(i2)){
                message("getting cached data")
                return(i2)
        }
        data<-x$get()
        i2<-solve(data,...)
        x$setinv(i2)
        i2
}
