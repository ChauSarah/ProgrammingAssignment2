## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: makes a special matrix object that can cache the inverse
## set and get the value of the matrix
## set and get the value of the inverse
        
makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {        
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) m_inv <<- solve
        get_inv <- function() m_inv
        list(set = set, get = get, set_inv = set_inv,get_inv = get_inv)
        }

## cacheSolve: computes inverse of the special "matrix" returned by makeCacheMatrix above 
## If inverse has already been calculated then the cachesolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # x is output of makeCacheMatrix()
        # returns inverse of original matrix input to makeCacheMatrix
        m_inv <- x$get_inv()
        
        # if inverse already calculated then get from cache and return
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        
        # otherwise, compute inverse
        data <- x$get()
        m_inv <- solve(data, ...)
        
        # set inverse value in cache
        x$set_inv(m_inv)
        m_inv
}
