## Function makeCacheMatrix creates a square matrix.
## The cacheSolve function computes the inverse matrix.

## makeCacheMatrix reads in a list of values and the number of rows(and columns)
## The function then forms a square matrix, which is stored in its environment,
## together with the get, set , getinverse and setinverse functions which is used 
## in the cacheSolve function.
##
## Please use the following commands to call the functions.
## Ensure that the amount of numerical values is a square number. Inverse
## Matrices are only performed on square matrices.
## mcm<-makeCacheMatrix(A<-c(2,4,10,12),n<-sqrt(length(A)))
## cacheSolve(mcm)

makeCacheMatrix<-function(A,n) {
  Ai <- NULL
 #1: get input values - matrix  
  set <- function(y) {  
    Ai <<- NULL
    A <<- y
    Ai <<- NULL
  } 
     get <- function() A<-matrix(A,nrow=n,ncol=n)
  setinverse <-function(solve)
    Ai<<-solve
  getinverse <-function()
    Ai
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The cachesolve function first checks for the presence of the inverse matrix.
## If present it retreives it from cache. Else it computes it.
## The purpose of this is to save computational time which is quite beneficial 
## when procession large amounts of data.

cacheSolve<-function(A,...){
 Ai<-A$getinverse()
 if(!is.null(Ai)) {
   message("getting cached data")
   return(Ai)
    }
   data <- A$get()
   Ai <- solve(data, ...)
  A$setinverse(Ai)
  Ai
}
