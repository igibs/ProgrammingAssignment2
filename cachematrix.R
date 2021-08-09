# Programming Assignment 2: Lexical Scoping

# This function creates a special matrix object that can cache its inverse.
# Specifically, it:
# 1) initializes the inverse of the matrix to "inv" and sets it to NULL;
# 2) sets the matrix to "x";
# 3) gets the matrix to "get";
# 4) sets the inverse of the matrix to "setInverse";
# 5) gets the inverse of the matrix to "getInverse"; and
# 6) gives the list of the set and get functions
makeCacheMatrix <- function(x=matrix()){
  #1
  inv <- NULL
  #2
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #3
  get <- function(){x}
  #4
  setInverse <- function(inverse){inv <<- inverse}
  #5
  getInverse <- function() {inv}
  #6
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# This function computes the inverse of the special matrix returned by the function above.
# Specifically, it:
# 1) grabs the stored inverse of "getInverse";
# 2) if the inverse exists, it returns it;
# 3) else it calculates, stores, and then returns it; and
# 4) prints the inverse
cacheSolve <- function(x,...){
  #1
  inv <- x$getInverse()
  #2
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #3
  mat <- x$get() 
  inv <- solve(mat,...)
  x$setInverse(inv)
  #4
  inv
}