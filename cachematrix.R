
## "makeCacheMatrix function" is giving off special "matrix" object cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL 
        set <- function(y) {
                x <<- y
                xinv <<- NULL # it also initialises xinv to null
        }
        get <- function() x # return the input matrix
        setInv <- function(inv) xinv <<- inv # set the inversed matrix
        getInv <- function() xinv # return the inversed matrix
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

##"cacheSolve" function creates the inverse of the special "matrix" returned by "makeCacheMatrix" function. 

cacheSolve <- function(x, ...) { m <- x$getInv() 
        if(!is.null(m)) { # if the inversion result is there
                message("getting cached data")
                return(m) 
        }
        data <- x$get() # if not, x$get to get the matrix object
        m <- solve(data) # solve it
        x$setInv(m) # set it to the object
        m # return the solved result
        ## Return a matrix that is the inverse of 'x'
}
