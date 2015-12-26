## Put comments here that give an overall description of what your
## functions do

# here is how makeVector and cachemean work ###########
# > a <- makeVector(c(1,2,3,4))
# > a$get()
# [1] 1 2 3 4 
# > a$getmean()
# NULL
# > cachemean(a)
# [1] 2.5
# > a$getmean()  # this is only to show you that the mean has been stored and does not affect anything
# [1] 2.5
# > cachemean(a)
# getting cached data
# [1] 2.5
# > a$set(c(10,20,30,40))
# > a$getmean()
# NULL
# > cachemean(a)
# [1] 25
# > cachemean(a)
# getting cached data
# [1] 25
# > a$get()
# [1] 10 20 30 40
# > a$setmean(0)  # do NOT call setmean() directly despite it being accessible for the reason you will see next
# > a$getmean()
# [1] 0      # obviously non-sense since...
# > a$get()
# [1] 10 20 30 40
# >cachemean(a)
# [1] 0    # as you can see the call to setmean() effectively corrupted the functioning of the code
# > a <- makeVector(c(5, 25, 125, 625))
# > a$get()
# [1] 5 25 125 625
# > cachemean(a)
# [1] 195
# > cachemean(a)
# getting cached data
# [1] 195

makeCacheMatrix <- function(x = matrix()) {
  #make list of functions that set and retrieve the inverse matrix
  m <- NULL #initially, the inverse is Null
  set <- function(y) { #the set function sets x of the parent to y. 
    x <<- y
    m <<- NULL #and resets the inverse to NULL, since x is new.
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve # solve inverts the matrix
  getinverse <- function() m
  list(set = set, get = get, #return a list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return from cache a matrix that is the inverse of 'x'
        ## OR, make an inverse and cache it.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get() #get the cached dataset
  m <- solve(data, ...) #solve with cached data
  x$setinverse(m) #reset cache
  m #return inverse
}
