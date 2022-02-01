## The first function defines a list of setter and getter functions that 
## will be called by the second function. 
## The second function calls on the getter and setter functions, and determines 
## whether to return an already chached inverse matrix, or to 
## compute and set a new cached value of the inverse of the input matrix. 



## The makeCasheMatrix function creates a list of function objects
## to be called in the second functino. The function objects created here
## include the setter and getter functions to set and retrieve the inverse 
## of the matrix from the input. This function also sets the initial empty
## matrix input, as well as the initial null value for the inverse function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function retrieves the stored inverse of the input matrix, and 
## see if it is null. It returns the stored value if it is there. But if the 
## initial value of the inverse is null, then the cacheSolve function computes
## the inverse of the input matrix, and sets it as the cached value. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve (data, ...)
        x$setinv(inv)
        inv
}
