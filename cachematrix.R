makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# x = rbind(c(1,3), c(2, 1))
#create a matrix - 2*2
# n = makeCacheMatrix(x)
# n$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    1
# retrievign inverse the first time
# cacheSolve(n)
#     [,1] [,2]
#[1,] -0.2  0.6
#[2,]  0.4 -0.2

# Retrieving the cache second time
# cacheSolve(n)
#getting cached data.
#     [,1] [,2]
#[1,] -0.2  0.6
#[2,]  0.4 -0.2
 


