## Written by John Barton 
## The functions in this file are used to  simulate a cache for calculating the inverse of a matrix
## A full description of each of the two functions is given below


#Test Data Used  is  shown below
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

#m1_inverse <- matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)

#m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

#m2_inverse<- matrix(c(3, 1, 7, 5), nrow = 2, ncol = 2)


#makeCacheMatrix when executed by using one of the matrices given above as a parameter
#will set up an environment containing four functions set, get, setinv and getinv and include the value
#of the matrix input as a parameter.
#eg.  a<-makeCacheMatrix(m1)  .........m1 must previously be entered into the global environment using command given above


makeCacheMatrix <- function(x = matrix()) {
     ix <- NULL                #initialize object used to store inverse matrix
     set <- function(y) {     
          x <<- y              #store y object entered as parameter in x object of parent environment
          ix <<- NULL          #store NULL in ix object of parent environment
     }
     get <- function() x
     setinv <- function(inv) ix <<- inv    
     getinv <- function() ix
     list(set = set, get = get,          #return a list to the function
          setinv = setinv,
          getinv = getinv)
     
}


## Write a short comment describing this function

#cacheSolve takes the output from makeCacheMatrix as a parameter
#It then calculates the inverse of the matrix entered in makeCacheMatrix
#if the calculation has been already performed then the value is obtained from the cache and message is displayed

cacheSolve <- function(x, ...) {
     
     ix <- x$getinv()              #get inverse from cache and store in ix object
     if(!is.null(ix)) {            #if value from cache was not NULL then use that data
          message("getting cached data")
          return(ix)               #return value from cache and exit function
          
     }
     #calculate value of inverse if cache was NULL
     data <- x$get()                 #get original matrix that was entered in makeCacheMatrix
     ix <- solve(data, ...)          #calculate inverse of matrix and store in object ix
     x$setinv(ix)                    #put inverse that was calculated in cache
     ix
     
}
