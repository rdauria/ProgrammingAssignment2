## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## makeCacheMatrix first checks whether the input matrix is square
## and if its determinant is different than 0 if either of this conditions
## are not mat it prints a hint and exits. In case the matrix is square and
## its determinant is different than 0 (an inverse of the matrix exists), 
## makeCacheMatrix creates a special matrix, a list, which components  
## are functions that:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse
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
## cacheSolve takes the special matrix object created by makeCacheMatrix
## and checks whether an inverse for the matrix passes as argument to 
## makeCacheMatrix already exists (checks whether the matrix does not
## contain NAs) if not it computes the inverse and sets in the cache

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
