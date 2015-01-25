## makeCacheMatrix and cacheSolve sets functions in memory environments to 
## improve function performance

## makeCacheMatrix is the creator function setting the environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
    setinverse <- function(solve) m<<- solve
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}
#cacheSolve

## cacheSolve is the returned function, which solves 
## for the inverse of a square matrix

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
  }

# #TESTS
# mat1 <- makeCacheMatrix()
# mat1$set(matrix(1:4, nrow = 2, ncol = 2))
# mat1
# inv1 <- cacheSolve(mat1)
# 
# mat2 <- diag(2)
# mat2
# 
# mat1 %*% mat2

