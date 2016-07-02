## makeCacheMatrix is the function that will use to storage in the cache  the matrix and its inverse using the second function

## This function will get a matrix and will return a list of four functions related to the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){	        ## This function will change the matrix x for a new one.
    x<<-y	                	## We use "<<-" in order to change de x valor in the general environment (not only in the function's environment)
    m<<-NULL	              ## If we change the x valor, then it changes the valor of its inverse, so we set the inverse (m) as NULL.
  }
  get<-function() x	        ## Get will return x (the matrix we want to calculate the inverse.
  setinv<-function(inv) m<<-inv ## setinv sets a valor (a matrix) that will be recognized as the inverse.
  getinv<-function()m	      ## Returns m (pressumably the inverse)
  list(set=set,get=get,setinv=setinv,getinv=getinv)	##In the end, makeCache Matrix return a list of this four functions we have defined.
}


## This function will get the list made by makeCacheMatrix and return its inverse.

cacheSolve <- function(x, ...) {
  m<-x$getinv()		    	## x will should be set like x<-makeCacheMatrix(t), if t is a matrix, so it's a list of functions related with t.
  if(!is.null(m)){			## If getinv is defined, it means that we already know the inverse, so we have finished, we return and exit de function.
    message("getting cached data")
    return(m)
  }
  data<-x$get()	    		## data will be the matrix.
  m<-solve(data,...)		## As it is not set, we find the inverse, (named m)..
  x$setinv(m)				    ## We save the inverse in the cache. (We will be able to find it by typing something like x$getinv
  m			            		## We return the inverse.
}
