## We first create a matrix using the makeCacheMatrix function. 
## Then we calculate the inverse of the matrix using the function cacheSolve

## This function creates a matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        #Function to set the matrix
        set <- function(matrix) {
                x <<- matrix
                i <<- NULL
        } 
        #Function to get the matrix
        get <- function(){
                x
        } 
        #Function to set the inverse of the matrix
        setInverse <- function(inverse){
                i <<- inverse
        } 
        #Function to get the inverse of the matrix
        getInverse <- function(){
                i
        }
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
## This function creates the inverse of the matrix that is passed as argument to the function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                #get cached data
                return inv
        } 
        m = x$get() 
        inv = solve(m) %*% m 
        x$setInverse(inv) 
        inv 
}
