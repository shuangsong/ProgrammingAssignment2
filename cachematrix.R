#This function is able to cache potentially time-consuming computations. 
#It takes the inverse of a numeric vector is typically a fast operation. 
#those two functions are time efficient to calculate big data.



#first create a cache function for matrix.
makeCacheMatrix<-function(x=matrix()) {
        m<-NULL
        #set value of matrix
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        #get value of matrix
        get<-function() x
        #set value of inverse
        setinverse<-function(solve) m<<-solve
        #get value of inverse
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#write cacheSolve function to return a matrix that is the inverse of 'x'
# it first checks to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation.
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.


cacheSolve <- function(x=matrix(), ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setinverse(m)
        m
}

