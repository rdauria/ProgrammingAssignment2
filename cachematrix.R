## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    if (dim(x)[1] != dim(x)[2]) {
        print ("Please provide a square matrix as input")
        return
    } else if (det(x)==0) {
        print ("Please provide a square matrix")
        print ("with determinant != than 0 as input")
        return
    }
    
    inv <- matrix(data=NA,nrow=dim(x)[1],ncol=dim(x)[1])
    set <- function(y){
        x <<- y
        inv <<- matrix(data=NA,nrow=dim(y)[1],ncol=dim(y)[1])
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set =set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!anyNA(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
