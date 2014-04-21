## Caching the Inverse Matrix
## ##########################
##
## Solution to the Programming Assignment 2
## of R-Programming (Johns Hopkins / Coursera)
##
## For very big matrices, it may take too long to
## compute the inverse, especially if it has to be computed repeatedly
## (e.g. in a loop).
## If the contents of a matrix are not changing, it may make
## sense to cache the inverse matrix for further use again. It
## can be looked up in the cache rather than recomputed. 

## This functions will take advantage of the scoping rules of the R
## language and how they can be manipulated to preserve state inside of an
## R object.
## Especially the the `<<-` operator is used, which assigns a value to
## an object in an environment that is different from the
## current environment. 
##############################

## The function, `makeCacheMatrix` creates a list containing
## a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- solve(x)
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##############################
## The following function calculates the inverse of a matrix
## created with the above function.
## However, it first checks to see if the inverse has already
## been calculated. If so, it `get`s the inverse from the
## cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value
## of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


##############################

## Testing All & Examples
#########################
## First Example
## x <- matrix(1:4, 2, 2)
## a1 <- makeCacheMatrix(x)
## a2 <- cacheSolve(a1)
## a2
## x %*% a2
## a2 %*% x

## Second Example
## x <- matrix(c(3:10, 4:11, 7:1, 3:4), 5, 5)
## a1 <- makeCacheMatrix(x)
## a2 <- cacheSolve(a1)
## a2
## x %*% a2
## a2 %*% x


