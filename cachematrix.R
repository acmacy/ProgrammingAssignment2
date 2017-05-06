## In this program, the invers of a matrix, which is a costly computation, is
## cached. If present in cache, it is retrieved from there, else it is calculated

## makeCacheMatrix function is used to Cache the Inverse Value.
## It is used for checking if value has been cached, and caching a new value everytime
## it is calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<-NULL
        }
        get<-function(){x}
        setinv<-function(i){
                inv<<-i
        }
        getinv<-function(){inv}
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function is the function that calculates the matrix inverse. It checks
## if value is present in cache. Then it uses that value. Otherwise it calculates it
## and returns it after caching it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(nrow(x$get())!=ncol(x$get())){
                return("Supplied matrix is not a square matrix, cannot compute inverse!")
        }
        inv<-x$getinv()
        if(!is.null(inv)){
                print("Fetching inverse of matrix from cache...")
                return(inv)
        }
        print("Calculating inverse of the matrix, please be patient...")
        data<-x$get()
        inv<-solve(data, ...)
        x$setinv(inv)
        inv
}
