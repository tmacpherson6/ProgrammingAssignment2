#Write a function that is able to cache potentially time-consuming computations.
#for example: taking the mean of a numeric vector is typically fast. if its bvery long, it may take too long to compute the mean especilly if it has to be computed in a loop.
#If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. 
#here, we will take advantage of scoping rules in the R language and how they can be manipulated to preserve state inside of an R object. 

#Here is an example of caching the mean of a vector:

makeVector <- function(x = numeric()) {
  m <- NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmean<-function(mean) m<<-mean
  getmean<-function() m
  list(set=set,get=get,
       setmean=setmean,
       getmean=getmean)
}
#This function create a vector which is a list containing a function to
#1) set the value of the vector
#2) get the value of the vector
#3) set the value of the mean
#4) get the value of the mean

#The following function calculates the mean of the special vector created above. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation
#otherwise it calculates the mean of the data and sets the value of the mean in the cahce via the setmean function.

cachemean<- function(x,...){
  m<-x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-mean(data,...)
  x$setmean(m)
  m
}