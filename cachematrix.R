# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()){
    
    inv <- NULL
    
    # Internal function to set the value of the matrix and initialize the inv
    set <- function(matrix){
        m <<- matrix
        inv <<- NULL
    }
    
    # Internal function to get the value of the matrix
    get <- function(){
        m
    }
    
    # Internal function to set the inverse of the matrix
    setInvMat <- function(inv_mat){
        inv <<- inv_mat
    }
    
    # Internal function to get the inverse of the matrix
    getInvMat <- function(){
        inv
    }

    # List of the methods
    list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
    
    # Getting the inverse matrix value
    inv_mat <- x$getInvMat()
    
    # Only returns the value if it has already been set.
    if(!is.null(inv_mat)){
        message("getting cached data")
        return(inv_mat)
    }
    
    # In case of the inverse of the matrix has never been calculated:
    # Getting the matrix value
    data <- x$get()
    
    # Computing the inverse of the matrix
    inv_mat <- solve(data)
    
    # Setting the inverse of the matrix
    x$setInvMat(inv_mat)
    
    # return value
    inv_mat
}

# Test
#> a1 <- c(3, 2, 5)
#> a2 <- c(2, 3, 2)
#> a3 <- c(5, 2, 4)
#> A <- rbind(a1, a2, a3)
#> A_special <- makeCacheMatrix(A)
#> cacheSolve(A_special)
#> cacheSolve(A_special)