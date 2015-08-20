## This script follows the examples closely. Two functions are created: 
## One creates a list of functions that can be used to set and get the inverted
## matrix for a matrix "x". The other one applies these functions to the list(!) 
## created in the first function and either sets or retreives the inverted matrix.

## Call this function with a square matrix [ncol==nrow] as argument.
## Inside the function a list of functions is defined which should be assigned to 
## an object (e.g. "a <- makeCacheVector(my_matrix)"). Because the resulting object
## is a list, its functions can be called by name (e.g. "a$get()") [...I assume =) ].
## The actual variables are saved in the function envirmonment [... again, I think], 
## not in the global one.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function takes the list created above as argument and either returns the 
## inverse matrix or sets it via the functions defined in the list.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## BONUS: Just a little function to validate my code:
## It compares the cached inverse matrix to the directly inverted matrix.
## Obvously that defies the purpose of this script (to save computation time)
## but as a simple tool for quick validation it helps.

testcorrect <- function(x,y) {  
    # x = the list created above (e.g. "a")
    # y = the original matrix (e.g. "my_matrix")
    
    correct <- all.equal(cacheInverse(x),solve(y)) 
    if (is.logical(correct) && (correct))  {
        message("---\n Everything correct!")
    } else {
        message(paste0("---\n Something is not right, it seems:\n",correct))
    }
}
