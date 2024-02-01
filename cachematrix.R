#Assignment 2 - Lexical Scoping
#Write the following functions
#1) makeCacheMatrix: this funcction creates a special matirx object that can cache its inverse
#2) cacheSolve: This function conmputes the inverse of the special matrix returned by makeCacheMatrix. 
    #If the inverse has already been calculated and not changed, then it should retrieve previous results.
#3) the solve() function can be used to calculate an inverted matrix.


## First function, set null inverse matrix, retrieve matrix, set list of functions

makeCacheMatrix <- function(x=matrix()) {
  #Start with a NULL inverse matrix
  ci <- NULL
  #set function updates teh matrix and resets the cached inverse
  set<-function(y) {
    x<<-y
    ci<<-NULL
  }
  
  #retrieve the matrix
  get<-function()x
  
  #setinverse function sets the cached inverse
  setinverse<-function(solve) ci<<-solve
  
  #getinverse retrieves the cached inverse
  getinverse<-function() ci
  
  #get the row and column lengths
  getnrow <- function() nrow(x)
  getncol <- function() ncol(x)
  
  #return a list of functions
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse,getnrow=getnrow,getncol=getncol)
}


## Design a function to check to see if we have a cached version, if not compute the inverse and cache it

cacheSolve<-function(x,...){
  #check to see if the inverse is already cached
  ci<-x$getinverse()
  if(!is.null(ci)){
    message("getting cached result")
    return(ci)
  }
  #if not, compute inverse
  data<-x$get()
  ci<-solve(data,...)
  
  #Cache the computed inverse
  x$setinverse(ci)
  
  #return the computed inverse
  ci
}  

##ran multiple times in R console with success.