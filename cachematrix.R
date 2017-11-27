## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function return a list of four functions

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve)

}


## Write a short comment describing this function
## This function is passed as an argument an object makeCacheMatrix, and evaluates
## wether the inverted matrix is already calculated(in cache) or not.If it's
## not, it calculates.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
}
