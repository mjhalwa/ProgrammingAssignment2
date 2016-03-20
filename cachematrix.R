## @brief functions are calculating inverse while caching

##
# This function creates a special "matrix" object
# that can cache its inverse.
#
# @see https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html
# The operators <<- and ->> are normally only used
# in functions, and cause a search to made through
# parent environments for an existing definition of
# the variable being assigned. If such a variable
# is found (and its binding is not locked) then its
# value is redefined
# , otherwise assignment takes place in the global
# environment.
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##
# This function computes the inverse of the special
# "matrix" returned by @see makeCacheMatrix above.
# - If the inverse has already been calculated
# - and the matrix has not changed
# then the cachesolve should retrieve the inverse
# from the cache.
##
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}