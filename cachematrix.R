## Matrix inversion computation is time consuming, so it is better to cache the inverse of a
## matrix if there is a need to use/compute them again for the same data. The following 
## function can compute and cache the inverse of a matrix.

## The first function,makeCacheMatrix is a special "matrix,which contains a list of function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix())   { 
	   inver <- NULL
	   set <- function(a)   {
		    x<<-a
		    inver<<- NULL
	}
		get<-function() x
		setinverse<- function(inverse) inver<<-inverse
		getinverse<- function() inver
		list(set=set , get=get ,
		setinverse=setinverse , getinverse=getinverse)
}

## The second function,cacheSolve computes and returns the inverse of the matrix created
## in the first function. Nonetheless, it first checks to see if the inverse has already been 
## computed. If so, it gets the inverse from the cache and skips computation. Otherwise, it
## computes the inverse of the matrix and sets the value of inverse by means of setinverse
## function. The matrix is assumed to be invertible in this function.

cacheSolve <- function(x, ...)     {
	   inver<-x$getinverse()
	   if(!is.null(inver))       {
		   message("getting cached data..")
		   return(inver)
       }
	   data<- x$get()
	   inver <-solve(data)
 	   x$setinverse(inver)
	   inver
}

## Example

## > x<-matrix(c(3,-3/4,-3/4,3),2,2)
## > c=makeCacheMatrix(x)
## > c$get()
##        [,1]    [,2]
## [1,]  3.00 -0.75
## [2,] -0.75  3.00
## > cacheSolve(c)
##          [,1]               [,2]
## [1,] 0.35555556 0.08888889
## [2,] 0.08888889 0.35555556
## > cacheSolve(c)
## getting cached data..
##          [,1]               [,2]
## [1,] 0.35555556 0.08888889
## [2,] 0.08888889 0.35555556