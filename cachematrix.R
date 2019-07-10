## makeCacheMatrix is used to contain the list of functions 
## namely get, set, getInverse and setInverse. It initialises the
##value of I (inverse) as NULL so that in each function call of makeCacheMatrix the value
##of I inside the function is NULL (not in global environment). Set() takes in the matrix
##and stores the value in global environment (GE). It also stores NULL for for I in GE
## get() returns current value of matrix. It is used while testing the condition of I being null
##setInverse() stores the calculated value to GE. getInverse() takes the value of I from GE

makeCacheMatrix <- function(x = matrix()) {
I<-NULL
set<-function(y){
  x<<-y
  I<<-NULL
}
get<-function() x
setInverse<-function(inv) I<<-inv
getInverse<-function() I
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The function takes value of I from GE, checks if it is NULL. If it is not NULL
## the cached value in GE is called. Otherwise the matrix is stored into data and its 
##inverse is calculated. setInverse function is used to store the value in GE

cacheSolve <- function(x, ...) {
  I<-x$getInverse()
  if(!is.null(I)){
    message("from cache")
    return(I)
  }
  data<-x$get()
  I<-solve(data, ...)
  x$setInverse(I)
  I
}
