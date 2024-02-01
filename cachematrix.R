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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
