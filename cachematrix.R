## Set of functions presented below allows to increase efficiency
## by caching the inverse of a matrix rather than computing it repeatedly.


# This function creates a special "matrix" object that can cache its inverse 
# x - is a matrix for which inverse need to be cached to speed up repeated calculation 
# Function output is list containing 4 functions


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  
  # set is 1st element in list used to set or updates value of x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get is 2nd element in list used to get matrix x for computations 
  get <- function() x
  
  # setinv is 3nd element in  list cashes inverse of x 
  setinv <- function(inverse) inv <<- inverse
  
  # getinv is 4th element in  list retrieves inverse of x from cash 
  getinv <- function() inv
  
  # names of list elements are defined in order to allow for {list name}${function name} syntax
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# Input should be list returned by makeCacheMatrix
# Values of matrix elements could be originally inputted in makeCacheMatrix or set/updated
# by {list name}$set function
# ... - could be used to pass additional arguments of solve function

cacheSolve <- function(x, ...) {
  # getting inverse from cash
  inv <- x$getinv()
  
  # returning inverse if it has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculating inverse if it has not been calculated
  data <- x$get()
  inv <- solve(data,...)
  
  # cashing inverse for future use
  x$setinv(inv)
  inv
}

