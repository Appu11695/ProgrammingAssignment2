##As we know that matrix inversion is time taking computation,and  there are some benefits
##of caching the inverse of a matrix rather than compute it repeatedly
##The functions below are used to cache the inverse of a matrix

## makeVector create a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix

 makeCacheMatrix<-function(p=matrix())
{
invm<-NULL
set<-function(y){
p<<-y
invm<<-NULL
}
get<-function() x
setinv<-function(inv)invm<<-inv
getinv<-function() invm
list(set=set,get=get,setinverse=setinv,getinverse=getinv)
}

## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
##computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve<-function(p,...)
{
invm<-p$getinv()
if(!is.null(invm))
{
message("getting cached data.")
return(invm)
}
data<-p$get()
invm<-solve(data)
p$setinv(invm)
invm
}

