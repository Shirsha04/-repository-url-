# -repository-url-
# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize a cache variable to store the matrix and its inverse
    cache <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        cache <<- NULL  # Clear the cache whenever the matrix is updated
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to compute the inverse of the matrix and cache it
    cache_inverse <- function() {
        if(!is.null(cache)) {
            message("Getting cached data")
            return(cache)
        }
        
        message("Calculating inverse and caching")
        inv <- solve(x)
        cache <<- inv
        inv
    }
    
    # Return a list of functions
    list(set = set,
         get = get,
         cache_inverse = cache_inverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    if(!is.null(x$cache_inverse())) {
        message("Inverse already cached")
        return(x$cache_inverse())
    }
    
    # If not, calculate the inverse and cache it
    x$cache_inverse()
}
