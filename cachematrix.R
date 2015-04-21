## Below functions are used to cache and compute the inverse of a matrix object
 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mt = matrix()) {
  
    mtInv<-NULL 
  
    set <- function(y) {
      mt <<- y      
      mtInv<<-NULL
    }
  
    get <- function() mt
    setInv <- function(solve) mtInv <<- solve
    getInv <- function() mtInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(mt, ...) {
      
    mtInv <- mt$getInv()
    if(!is.null(mtInv)) {
      message("getting cached data")
      return(mtInv)
    }
    
    data <- mt$get()
    mtInv <- solve(data, ...)
    mt$setInv(mtInv)
    return(mtInv)
}
