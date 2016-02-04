## These functions take a matrix as argument and cache its inverse


## This function creates a "special matrix" which is in
## fact a list of functions to: (a) set and get the value of matrix
## and (b) set and get the inverse of that matrix 

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
		}
	get<-function()x
	setinv<-function(inv) i<<-inv
	getinv<-function()i
	
	list(set = set, get= get,setinv =setinv,getinv=getinv)

}


## This function calculates the inverse of a matrix obtained from
## the previous function by using the cached inverse matrix
## if it has been calculated and cached before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i<-x$getinv()
        if(!is.null(i)){
        	message("getting cached inverse")
        	return(i)
        }
        m<-x$get()
        i<-solve(m)
        i<-x$setinv(i)
        i
        
}
