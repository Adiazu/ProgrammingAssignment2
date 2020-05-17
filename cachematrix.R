


## This function cache the inverse of a matrix.
## The first function creates a special list containing differents functions that will help saving the cache or setting a new value to a matrix



makeCacheMatrix <- function(x = matrix()) { ## Here you have to input a Matrix, for example makeCacheMatrix(matrix(1:4,2,2))  
    S <- NULL
    set <- function(y=numeric,nrows,ncol) { ## This function set a new matrix if you want to, yo have to put the numbers and the numbers of columns and rows
        x <<- matrix(y,nrows,ncol)
        S <<- NULL
    }
    get <- function() x
    setSolve <- function(Solve) S <<- Solve
    getSolve <- function() S
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function is the one that check if the inverse of the matrix had been already calculated

cacheSolve <- function(x, ...) { 
    S <- x$getSolve()
    if(!is.null(S)) {
        message("getting cached data")
        return(S)
    }
    data <- x$get()
    S <- solve(data, ...)
    x$setSolve(S)
    S
}