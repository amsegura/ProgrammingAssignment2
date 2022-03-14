## Programming assignment2
## A function to create a matrix, compute and cache its inverse is created.

## This function creates the special "matrix" object that can cached its
## inverse. This object is a list that allows to retrieves values 
## calling by name the different elements in the list. 
makeCacheMatrix <- function(x = matrix()) {
        i2<- NULL
        ## Below the function that set value to the matrix and cached it
        set<- function(y){
                x<<-y
                i2<<-NULL
        }
        get<- function() x ## This part of the function gets the matrix value
        setinv<-function(inv) i2<<- inv ## Sets the inverse and cached it
        getinv<- function() i2 ## Retrieves the inverse
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if the matrix passed has already an inverse calculated.
## If so, retrieves from cache that value. If not, calculates and cached the 
## inverse of that matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First checks if it has already calculated and cached
        i2<- x$getinv()
        ## In that case retrieves it
        if(!is.null(i2)){
                message("getting cached data")
                return(i2)
        }
        ## If no, inverse is cached this part calculate it
        data<-x$get()
        i2<-solve(data,...)
        x$setinv(i2)
        i2
}
