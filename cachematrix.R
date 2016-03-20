## @brief functions are calculating inverse while caching

##
# @brief creates special "matrix" object able to cache its inverse
# @param x some matrix
# @return special "matrix" able to cache its inverse
#
# this matrix can be used with @see cacheSolve to calculate
# inverse of a provided matrix - if not yet calculated.
# An already calculated inverse matrix is stored within this
# object to prevent double-calculations.
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
# @brief compute inverse of a matrix while looking for cached results
# @param x matrix created by @see makeCacheMatrix
# @return inverse of matrix x
#
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

##
# @brief generating a list of square matrices
# @param len length of list = amount of matrices
# @param squaredim dimension to create a squaredim x squaredim matrix
# @return list of squaredim x squaredim matrices
##
generate_test_list <- function(len,squaredim=3) {
  randMatrix <- function(rows,cols) {
    matrix(rnorm(rows*cols),ncol=cols,nrow=rows)
  }
  mapply(randMatrix,rep(squaredim,len),rep(squaredim,len),SIMPLIFY=FALSE)
}

##
# @brief function to compare time consumption
# @param len length of list = amount of matrices @see generate_test_list
# @param squaredim dimension to create a squaredim x squaredim matrix @see generate_test_list
# 
# provides output about time consumption
##
test <- function(len=4,squaredim=3) {
  M <- generate_test_list(len,squaredim)
#  print(M)
  specialM <- lapply(M, makeCacheMatrix)

  #normal solve
  t <- as.POSIXlt(Sys.time())
  solvedM <- lapply(M,solve)
  tEnd <- as.POSIXlt(Sys.time())
  print("normal: ")
  print(tEnd-t, units="seconds")
  
  #1st cacheSolve
  t <- as.POSIXlt(Sys.time())
  solvedM <- lapply(specialM,cacheSolve)
  tEnd <- as.POSIXlt(Sys.time())
  print("1st cacheSolve: ")
  print(tEnd-t, units="seconds")
  
  #2nd cacheSolve
  t <- as.POSIXlt(Sys.time())
  solvedM <- lapply(specialM,cacheSolve)
  tEnd <- as.POSIXlt(Sys.time())
  print("2nd cacheSolve: ")
  print(tEnd-t, units="seconds")
  
#  print(solvedM)
  mapply(function(X,Y){X%*%Y},M,solvedM,SIMPLIFY=FALSE)
  print("done")
}