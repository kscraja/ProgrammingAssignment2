
## Inverting a Large matrix is an computationallly expensive process.
## If the underlying matrix-data is unchanging, it would be better
## to cache the 'inverted cache' value and serve it next instead of computing afresh

## It can be handled by creating a special 'matrix' data-structure with capacity 
## to fetch and store (both the data and its cache value)

## This function contructs a data structure representing 
## data, and its cached value, along with functions to get/set those values
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL #cached value
    set <- function(y) {
        x <<- y ## updating the x in the outer env, instead of creating a new binding
        cache <<- NULL ## resetting the cached value
    }
    get <- function() x
    setCache <- function(c) cache <<- c ## setting the cache value
    getCache <- function() cache
    list(set=set, get=get, setCache=setCache, getCache=getCache)
}


## users should first construct wrapped matrix-data structure using the above makecachematrix
## instead of calling Solve of default matrix, cacheSolve can be used.
## this will compute the inverted value ## only for the first time after setting of the data. 
## Rest of invocations will be just returning the previously computed value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cache <- x$getCache()
    if(is.null(cache)) {
        print("Computing inverted matrix")
        # cache is not yet computed
        data <- x$get() # fetching the data
        cache <- solve(data, ...) # computing the required value. i.e inverted matrix
        x$setCache(cache) # setting the cache in the data structure
    }
    cache
}